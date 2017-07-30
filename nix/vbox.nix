{ machine = {
    deployment = {
      targetEnv = "virtualbox";

      virtualbox.headless = true;

      virtualbox.memorySize = 2048;  # Increase this number for more memory
    };
  };
}
