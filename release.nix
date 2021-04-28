let
  default = import ./default.nix { };

in
  { CPP =
      default.aggregate
        { name = "all";

          constituents = [
            default.CPP
          ];
        };
  }
