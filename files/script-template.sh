#!/bin/sh

exit_ok=0
exit_usage=1

usage() {
    cat<<EOF
usage: $1 [OPTIONS]... [ARGS]...
options:
    -h       this help
EOF
}

while getopts "h" opt
do
    case "${opt}" in
        h)
            usage
            exit "${exit_ok}"
            ;;
        *)
            2>&1 usage
            exit "${exit_usage}"
            ;;
    esac
done

shift $((OPTIND - 1))
