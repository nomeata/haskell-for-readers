set -e

function check () {
	aspell -l $1 -p ./wordlist \
	  -c $2
}
check en_US haskell-for-readers.md
