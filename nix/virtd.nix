{ machine =
    { deployment.targetEnv = "libvirtd";
      deployment.libvirtd = {
        headless = true;

        imageDir = "/var/lib/libvirt/images";

        memorySize = 2048;  # Increase this number for more memory
      };
    };
}
