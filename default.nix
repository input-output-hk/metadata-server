{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-shell '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
# GitHub PR number (as a string), set when building a Hydra PR jobset.
, pr ? null
# Bors job type (as a string), set when building a Hydra bors jobset.
, borsBuild ? null
}:
with pkgs; with commonLib;
let

  src = lib.cleanSourceWith {
    src = pkgs.haskell-nix.cleanSourceHaskell { src = ./.; };
    name = "metadata-server-src";
  };

  buildHaskellProject = args: import ./nix/haskell.nix ({
    inherit config pkgs;
    inherit (pkgs) buildPackages lib stdenv haskell-nix;
    inherit src gitrev pr borsBuild;
    compiler = config.haskellNix.compiler or "ghc8102";
  } // args);
  project = buildHaskellProject {};
  profiledProject = buildHaskellProject { profiling = true; };

  haskellPackagesMusl64 = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages pkgs.pkgsCross.musl64.metadataServerHaskellPackages);
  metadataValidatorGitHubTarball = pkgs.runCommandNoCC "metadata-validator-github-tarball" { buildInputs = [ pkgs.gnutar gzip ]; } ''
    cp ${haskellPackagesMusl64.metadata-validator-github.components.exes.metadata-validator-github}/bin/metadata-validator-github ./
    mkdir -p $out/nix-support
    tar -czvf $out/metadata-validator-github.tar.gz metadata-validator-github
    echo "file binary-dist $out/metadata-validator-github.tar.gz" > $out/nix-support/hydra-build-products
  '';
  nixosTests = recRecurseIntoAttrs (import ./nix/nixos/tests {
    inherit pkgs;
  });
  docScripts = pkgs.callPackage ./docs/default.nix { };

  self = {
    # New
    inherit pkgs commonLib src project profiledProject;

    inherit (project.hsPkgs.metadata-server.identifier) version;
    inherit (project.hsPkgs.metadata-server.components.exes) metadata-server;
    inherit (project.hsPkgs.metadata-webhook.components.exes) metadata-webhook;
    inherit (project.hsPkgs.metadata-validator-github.components.exes) metadata-validator-github;

    inherit metadataValidatorGitHubTarball;
    inherit nixosTests docScripts;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" project.hsPkgs;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" project.hsPkgs;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks project.hsPkgs;
    };

    shell = import ./shell.nix { inherit pkgs; metadataPackages = self; withHoogle = true; };
    shell-prof = import ./shell.nix { inherit pkgs; metadataPackages = self; withHoogle = true; profiling = true; };
    
    inherit (pkgs.iohkNix) checkCabalProject;
  };
in self
