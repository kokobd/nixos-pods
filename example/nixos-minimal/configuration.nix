{ pkgs, config, modulesPath, ... }:
{
  imports = [
    "${modulesPath}/virtualisation/amazon-image.nix"
  ];

  nix.package = pkgs.nixFlakes;
  system.stateVersion = "22.11";

  environment.systemPackages = with pkgs; [
    (vim_configurable.customize {
      name = "vim";
      vimrcConfig.customRC = ''
                  filetype plugin indent on
                  set expandtab
                  set tabstop=2
                  set nu
                  set softtabstop=2
                  set shiftwidth=2
                  set autoindent
        	'';
    })
    vim
    git
    curl
    screen
  ];

  programs.vim.defaultEditor = true;
  services = {
    openssh = {
      enable = true;
      passwordAuthentication = false;
    };
  };

  users = {
    mutableUsers = false;
    users = {
      example = {
        isNormalUser = true;
        description = "Example User";
        extraGroups = [ "networkmanager" "wheel" ];
        # plaintext: 111
        hashedPassword = "$6$tvEUbZ9fc/6wMeHH$v5xTGK.gUQo1JPB8i6bL3tUuYUqBkuxejSeSxAZBp2sgix4cLudvWopc5nMnNvbYE2un5LJfzeUeaZ1tEOyG9/";
        openssh.authorizedKeys.keys = [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINwO/CamuWMIrjKaSXDyqQ2hQOU77XHRoxHOj9//SSTM" ];
      };
    };
  };
}
