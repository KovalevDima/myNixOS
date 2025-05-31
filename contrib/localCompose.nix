{executable, inputs, pkgs}:
{
  imports = [inputs.services-flake.processComposeModules.default];
  services.clickhouse."database" = {
    enable = true;
    extraConfig.http_port = 8123;
    initialDatabases = [ {name="default";} ];
  };
  settings.processes = {
    "executable" = {
      command = ''
        ${executable.program} +RTS -A32m -RTS
      '';
      depends_on."database".condition = "process_healthy";
    };
  };
  services.grafana."grafana" = {
    enable = true;
    http_port = 8080;
    datasources = [
      {
        name = "ClickHouse";
        type = "grafana-clickhouse-datasource";
        jsonData = {
          port = "9000";
          host = "localhost";
        };
      }
    ];
    declarativePlugins = [
      pkgs.grafanaPlugins.grafana-clickhouse-datasource
    ];
  };
}
