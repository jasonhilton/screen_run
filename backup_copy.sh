local_path=$1
remote_path=/research/CPCModelling/screen_run/$1

ssh jdh1d15@ssh.soton.ac.uk remote_path=$remote_path "mkdir -p $remote_path"
scp $local_path/* jdh1d15@ssh.soton.ac.uk:$remote_path
