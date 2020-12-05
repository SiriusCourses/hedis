{ pkgs
, dockerTools
, busybox
, iana-etc
, tini
, gettext
, glibcLocales-light
, cacert
}:
dockerTools.buildImage {
  name = "sirius-base";
  contents = [ busybox iana-etc tini cacert gettext ];

  config = {
    Env = [
      "PATH=/bin"
      "LOCALE_ARCHIVE=${glibcLocales-light}/lib/locale/locale-archive"
      "LC_ALL=en_US.UTF-8"
      "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
    ];

  };

}
