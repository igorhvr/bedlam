var iasylum;
var LZString;

if (!iasylum || !LZString) {
    var dependence = 'compression.js depends on iasylum.js from bedlam and LZString from the lz-string.js library.';
    console.error(dependence);
    throw dependence;
}

var iasylum_compression = {
    'lzw_compress': function (data) {
        return LZString.compressToBase64(data);
    },

    'lzw_decompress': function (data) {
        return LZString.decompressFromBase64(data);
    }

};

iasylum.compression = iasylum_compression;
