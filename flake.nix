{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = inputs:
    with inputs; let
      supportedSystems = [ "x86_64-linux" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system:
        import nixpkgs {
          inherit system;
          overlays = [];
        });

    in {
      devShell = forAllSystems (system:
      nixpkgsFor.${system}.mkShell {
        packages = with nixpkgsFor.${system}; [
          imagemagick
          (ghc.withPackages (ps: with ps; [
            MissingH
            aeson
            blaze-html
            clay
            hakyll
            pkgs.cabal-install
            string-conv
            tagsoup
            time-locale-compat
          ]))
        ];
      }
      );
    };
}
