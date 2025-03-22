{
  description = "flake to run nyxt config as kiosk under asahi/nixOS +gamescope";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    nyxt-git.url = "github:atlas-engineer/nyxt";
  };
  outputs = {
    self,
    nixpkgs,
    ...
  } @ inputs: let
    inherit (self) outputs;
  in {
    overlays = final: prev: {
      otf-apple = prev.callPackage ./derivations/otf-apple.nix { };
      sf-mono-liga-bin = prev.callPackage ./derivations/sf-mono-liga-bin.nix { };
      nyxt4 = prev.callPackage ./derivations/nyxt-4.0.0-pre-release-3.nix { };
    };
    nixosConfigurations = {
      shaurizard = nixpkgs.lib.nixosSystem {
        specialArgs = {inherit inputs outputs;};
        modules = [
          ./nixos/configuration.nix
        ];
      };
    };
  };
}

