{
  pkgs,
  lib,
  config,
  ...
}:
{
  languages = {
    r.enable = true;
    r.lsp.enable = true;
  };

  packages = [
    pkgs.rPackages.tidyverse
  ];
}
