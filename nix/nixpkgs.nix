{ fetchTarball ? builtins.fetchTarball }:

# nixpkgs release 21.11 pinned on September 8th, 2022.
# url: <https://github.com/NixOS/nixpkgs/releases/tag/22.05>
fetchTarball {
  url = "https://github.com/NixOS/nixpkgs/archive/refs/tags/22.05.tar.gz";
  sha256 = "0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
}