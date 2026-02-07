# Tailscale VPN Configuration
# Provides secure mesh networking for accessing services from mobile devices
{ config, pkgs, ... }:

{
  # Enable Tailscale daemon
  services.tailscale.enable = true;

  # Firewall configuration for Tailscale
  networking.firewall = {
    # Trust all traffic from Tailscale network
    trustedInterfaces = [ "tailscale0" ];

    # Allow Tailscale's UDP port for direct connections
    allowedUDPPorts = [ config.services.tailscale.port ];
  };

  # Tailscale CLI for authentication and management
  environment.systemPackages = [ pkgs.tailscale ];
}
