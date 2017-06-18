let
  adminPasswordPath = "/tmp/rabbitmq/adminPassword";

  serverPasswordPath = "/tmp/rabbitmq/serverPassword";

  setupCompletePath = "/tmp/rabbitmq/setupComplete";

  indexPath = "/tmp/lib/suns";

in
{ config, pkgs, ... }: {
  nixpkgs = {
    config = import ./config.nix;

    system = "x86_64-linux";
  };

  environment.systemPackages = [ pkgs.haskellPackages.suns-search ];

  networking.firewall.allowedTCPPorts =
    config.services.openssh.ports ++ [ config.services.rabbitmq.port ];

  services.rabbitmq = {
    enable = true;

    listenAddress = "";
  };

  systemd.services = {
    rabbitmq = {
      postStart = ''
        if [ ! -e ${adminPasswordPath} ]; then
          mkdir -p ${dirOf adminPasswordPath}
          ${pkgs.pwgen}/bin/pwgen -s 20 > ${adminPasswordPath}
        fi
        if [ ! -e ${serverPasswordPath} ]; then
          mkdir -p ${dirOf serverPasswordPath}
          ${pkgs.pwgen}/bin/pwgen -s 20 > ${serverPasswordPath}
        fi
        if [ ! -e ${setupCompletePath} ]; then
          mkdir -p ${dirOf setupCompletePath}
          ${pkgs.rabbitmq_server}/bin/rabbitmqctl add_vhost suns-vhost
          ${pkgs.rabbitmq_server}/bin/rabbitmqctl add_user suns-admin $(< ${adminPasswordPath})
          ${pkgs.rabbitmq_server}/bin/rabbitmqctl add_user suns-server $(< ${serverPasswordPath})
          ${pkgs.rabbitmq_server}/bin/rabbitmqctl add_user suns-client suns-client
          ${pkgs.rabbitmq_server}/bin/rabbitmqctl set_permissions -p suns-vhost suns-admin "^suns-*" "^suns-*" "^suns-*"
          ${pkgs.rabbitmq_server}/bin/rabbitmqctl set_permissions -p suns-vhost suns-server "^$" "^suns-exchange-responses$" "^suns-queue-.*"
          ${pkgs.rabbitmq_server}/bin/rabbitmqctl set_permissions -p suns-vhost suns-client "^amq\.gen.*" "^amq\.gen.*|suns-exchange-requests$" "^amq\.gen.*|suns-exchange-responses$"
          ${pkgs.haskellPackages.suns-search}/bin/suns-admin < ${adminPasswordPath}
          touch ${setupCompletePath}
        fi
      '';

      serviceConfig = {
        Restart = "on-failure";

        RestartSec = "10s";
      };
    };

    suns-server = {
      description = "Serve SUNS search requests";

      wantedBy = [ "multi-user.target" ];

      requires = [ "rabbitmq.service" ];

      after = [ "rabbitmq.service" ];

      preStart = ''
        for i in {1..3}; do
            test -e ${setupCompletePath} && break
            echo "Waiting for RabbitMQ setup to complete.  Checking again in 7 seconds"
            sleep 7
        done
        if [ ! -e ${indexPath} ];then
            mkdir -p ${indexPath}
            ${pkgs.haskellPackages.suns-search}/bin/suns-index --motifs ${../motif} --pdbs ${../pdb} --index ${indexPath}
        fi
      '';
 
      script = ''
        ${pkgs.haskellPackages.suns-search}/bin/suns-server --timeout 10000 --index ${indexPath} < ${serverPasswordPath}
      '';
    };
  };
}
