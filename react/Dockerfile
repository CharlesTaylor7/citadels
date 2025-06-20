FROM node:20-slim AS builder
WORKDIR /app
COPY package.json yarn.lock .yarnrc.yml ./
# Use cache mount for node_modules to speed up installation
RUN --mount=type=cache,target=/root/.cache/yarn \
    corepack enable && yarn install --immutable
COPY . .
# force IPv4 host instead of IPv6
ENV NITRO_HOST="0.0.0.0"
ENV NITRO_PORT="3000"
ARG COMMIT_SHA
ENV VITE_COMMIT_SHA=${COMMIT_SHA}
RUN yarn build
FROM node:20-slim AS production
RUN apt-get update \
  && apt-get install -y libsqlite3-dev \
  && rm -rf /var/lib/apt/lists/*
WORKDIR /app
COPY --from=builder /app/.output ./.output
COPY --from=builder /app/drizzle ./drizzle
EXPOSE 3000
CMD ["node", ".output/server/index.mjs"]
