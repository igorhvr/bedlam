var iasylum;
var LZString;

if (!iasylum || !LZString) {
    var dependence = 'compression.js depends on iasylum.js from bedlam and LZString from the lz-string.js library.';
    console.error(dependence);
    throw dependence;
}

var iasylum_compression = {
    'lzw-compress': function (data) {
        return LZString.compress(data);
    },

    'lzw-uncompress': function (data) {
        return LZString.decompress(data);
    }

};

iasylum.compression = iasylum_compression;
