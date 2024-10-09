docker build -t iccat/apps/nc_dashboard:0.1.0 \
             --build-arg GITHUB_AUTH_TOKEN=$GITHUB_AUTH_TOKEN \
             --progress=plain \
             -f Dockerfile.arm .
             
