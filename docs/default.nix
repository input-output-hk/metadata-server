{ emacs, git, cardano-cli, cardano-metadata-submitter, runCommand, lib, coreutils, gawk, curl, jq }:

let
  buildInputs =
    [ emacs
      git
      cardano-metadata-submitter
      cardano-cli
      coreutils
      gawk
      curl
    ];
in
  runCommand
    "generate-documentation-scripts"
    { inherit buildInputs; }
    ''
      mkdir -p $out/bin
      cd $out/bin

      cp ${./main.org} ./main.org
      emacs --batch --eval "(require 'org)" --eval '(org-babel-tangle-file "./main.org")'
      mv ./main.org $out/main.org
      bash ./mk-nix-setup.sh
      mv metadata-config.nix $out/
    ''
