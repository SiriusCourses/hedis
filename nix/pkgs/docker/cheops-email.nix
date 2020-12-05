{ pkgs
, dockerTools
, cheops-email-exe
, glibcLocales-light
}:
dockerTools.buildImage {
  fromImage = pkgs.sirius-base;
  name = "registry.sirius.online/cheops-edu/cheops-email/cheops-email";
  tag = "latest";
  contents = [ cheops-email-exe.chemail pkgs.busybox ];
  config.Cmd = [ "cheops-email" ];
  config.Env = [
    "LOCALE_ARCHIVE=${glibcLocales-light}/lib/locale/locale-archive"
    "LC_ALL=en_US.UTF-8"
    "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
  ];
}

