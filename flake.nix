{
  description = "geniusyield-orderbot";
  inputs.haskell-nix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
  inputs.plutus.url = "github:input-output-hk/plutus"; # used for libsodium-vrf

  # Latest cardano-node: 1.35.2
  inputs.cardano-node.url = "github:input-output-hk/cardano-node?ref=1.35.3";

  outputs = { self, nixpkgs, haskell-nix, plutus, cardano-node }:
    let
      supportedSystems = [ "x86_64-linux" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;

          # For adding cardano exes to the nix shell.
          cardano-exes = [
            cardano-node.apps.${system}.cardano-node
          ] ++ (with cardano-node.apps.${system}; [
            cardano-cli
          ]);
          cardanoExesPath = builtins.concatStringsSep ":" (map (x: builtins.dirOf x.program) cardano-exes);

          project = pkgs.haskell-nix.project' {
            src = ./.;
            compiler-nix-name = "ghc8107";
            projectFileName = "cabal.project";
            modules = [{
              packages = {
                plutus-use-cases.flags.defer-plugin-errors = deferPluginErrors;
                plutus-ledger.flags.defer-plugin-errors = deferPluginErrors;
                plutus-contract.flags.defer-plugin-errors = deferPluginErrors;
                cardano-crypto-praos.components.library.pkgconfig =
                  nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
                cardano-crypto-class.components.library.pkgconfig =
                  nixpkgs.lib.mkForce [ [ (import plutus { inherit system; }).pkgs.libsodium-vrf ] ];
              };
            }];
            shell = {
              withHoogle = true;

              nativeBuildInputs = (with pkgs; [
                cabal-install
                hlint
                fd
                bashInteractive
                jq
                haskellPackages.fourmolu
                gnumake
                gnused
              ]);

              tools = {
                haskell-language-server = { };
              };

              shellHook = ''
                export PATH=$PATH:${cardanoExesPath}
                export LC_CTYPE=C.UTF-8
                export LC_ALL=C.UTF-8
                export LANG=C.UTF-8
              '';
            };
          };
        in
        project;
    in
    {
      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      packages = perSystem (system: self.flake.${system}.packages);
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system:
        let
          pkgs = nixpkgsFor system;
          devShell = self.flake.${system}.devShell;
        in
        pkgs.lib.overrideDerivation (devShell) (oldAttrs: {
          buildInputs = pkgs.lib.lists.unique devShell.buildInputs;
        })
      );
    };
}
