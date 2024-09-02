# Use an official Node runtime as the parent image
FROM node:14

# Set the working directory in the container to /app
WORKDIR /app

# Copy package.json and package-lock.json to the working directory
COPY package*.json ./

# Install any needed packages specified in package.json
RUN npm install

# Bundle app source inside the docker image
COPY . .

# Build the app using the environment variables provided
# another option is to not build the site now and build it later when the container is run, so env variables can be passed at runtime
ARG BLOCKFROST_PREVIEW
ENV BLOCKFROST_PREVIEW=${BLOCKFROST_PREVIEW}

# Build the app
RUN npm run build

# Make port 3000 available to the world outside this container
EXPOSE 3000

# Define the command to run your app using CMD which defines your runtime
CMD [ "npm", "start" ]