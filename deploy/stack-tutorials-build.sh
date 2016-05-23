export SITE_ROOT_URL=//staging.stackbuilders.com
export SITE_PROTOCOL=https:
export PATH=~/.local/bin:/user/local/bin:$PATH
stack setup
make rebuild
