DOCKER_TAG='latest'

# log into docker hub.
docker login -u $DOCKER_USERNAME -p $DOCKER_PASSWORD

# build the docker image.
docker build -f ./Dockerfile -t bejebeje/elm:$DOCKER_TAG -t bejebeje/elm:$TRAVIS_BUILD_NUMBER --build-arg FONTAWESOME_NPM_AUTH_TOKEN=$FONTAWESOME_NPM_AUTH_TOKEN --build-arg IDENTITY_AUTHORITY=$IDENTITY_AUTHORITY --build-arg IDENTITY_CLIENT_ID=$IDENTITY_CLIENT_ID --build-arg IDENTITY_REDIRECT_URI=$IDENTITY_REDIRECT_URI --build-arg IDENTITY_RESPONSE_TYPE=$IDENTITY_RESPONSE_TYPE --build-arg IDENTITY_SCOPE=$IDENTITY_SCOPE --build-arg IDENTITY_POST_LOGOUT_REDIRECT_URI=$IDENTITY_POST_LOGOUT_REDIRECT_URI . --no-cache

# tag the docker image with latest.
docker tag bejebeje/elm:$DOCKER_TAG $DOCKER_USERNAME/bejebeje/elm:$DOCKER_TAG

# tag the docker image with build number.
docker tag bejebeje/elm:$DOCKER_TAG $DOCKER_USERNAME/bejebeje/elm:$TRAVIS_BUILD_NUMBER

# push the docker image (tagged latest) to docker hub.
docker push bejebeje/elm:$DOCKER_TAG

# push the docker image (tagged with build number) to docker hub.
docker push bejebeje/elm:$TRAVIS_BUILD_NUMBER