let
  region = "us-west-1";  # Change this to an AWS availability zone near you

  accessKeyId = "default";

in
  { machine = { resources, ... }: {
      deployment = {
        targetEnv = "ec2";

        ec2 = {
          inherit accessKeyId region;

          inherit (resources.ec2KeyPairs) keyPair;

          ebsInitialRootDiskSize = 10;

          instanceType = "t2.micro";  # Change this if you want a beefier server
        };
      };
    };

    resources.ec2KeyPairs.keyPair = { inherit region accessKeyId; };
  }
