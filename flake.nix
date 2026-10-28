{
  description = "sap.hx: see what changed in Helix -- inline git diffs, a changes buffer, hunk staging (Steel plugin)";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { self, nixpkgs }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      forAll = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
      version = "0.1.0";
    in
    {
      packages = forAll (pkgs: rec {
        # The cog, laid out the way helix-plugins-nix and forge expect;
        # `passthru.cogName` lets helix-plugins-nix's module install it.
        # Needs a Steel helix with dna.hx's virtual-text patch.
        sap = pkgs.stdenvNoCC.mkDerivation {
          pname = "helix-plugin-sap";
          inherit version;
          src = pkgs.lib.fileset.toSource {
            root = ./.;
            fileset = pkgs.lib.fileset.unions [
              ./cog.scm
              ./sap.scm
              ./util.scm
            ];
          };
          dontBuild = true;
          installPhase = ''
            mkdir -p $out
            install -m644 *.scm $out/
          '';
          passthru.cogName = "sap";
          meta = {
            description = "See what changed in Helix: inline git diffs, a changes buffer, hunk staging";
            homepage = "https://github.com/vitaliikindrat/sap.hx";
            license = pkgs.lib.licenses.mit;
          };
        };
        default = sap;
      });

      checks = forAll (pkgs: {
        steel-tests = pkgs.runCommand "sap-steel-tests" { nativeBuildInputs = [ pkgs.steel ]; } ''
          cp -r ${./.}/. .
          chmod -R u+w .
          sh tests/run-all.sh
          touch $out
        '';
      });
    };
}
