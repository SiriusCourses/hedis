{ cheops-email
, writeScript
, stdenv
}: 

stdenv.mkDerivation
  { name = "cheops-email-exe";
    src = cheops-email;
    builder = writeScript "cheops-email-exe-builder"
     ''
    source $stdenv/setup
    mkdir -p $chemail/bin
    for i in cheops-email ; do
      cp ${cheops-email}/bin/$i $chemail/bin/$i
      chmod +x $chemail/bin/$i
    done

    touch $out
      '';
    outputs = [
      "out" # TODO: get rid of this
      "chemail"
    ];
  }
  
