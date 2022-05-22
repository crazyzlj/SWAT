# SWAT on docker

The SWAT images have been built for linux:arm64 and linux:amd64, and pushed to
the [Docker hub](https://hub.docker.com/r/crazyzlj/swat/tags).

## Usage

```shell
docker pull crazyzlj/swat:apline-<VERSION_MAJOR>.<VERSION_MINOR>
docker run -it -v /path/to/your/swat-model-data:/swat_data swat<VERSION_MAJOR>.<VERSION_MINOR>.gfort.rel

# For example,
docker pull crazyzlj/swat:apline-2012.682
docker run -it -v /Users/ljzhu/Documents/tmp/swatdata:/swat_data crazyzlj/swat:alpine-2012.682 swat2012.682.gfort.rel
```