docker build -t iccat/apps/nc_dashboard:0.1.0 \
             --build-arg GITLAB_AUTH_TOKEN=$GITLAB_AUTH_TOKEN \
             --progress=plain \
             .
