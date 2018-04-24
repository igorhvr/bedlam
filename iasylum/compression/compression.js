var iasylum;
var LZString;

if (!iasylum || !LZString) {
    var dependence = 'compression.js depends on iasylum.js from bedlam and LZString from the lz-string.js library.';
    console.error(dependence);
    throw dependence;
}

var iasylum_compression = {
    'lzw_compress': function (data) {
	result=LZString.compressToUTF16(data);
	return result;
    },

    'lzw_decompress': function (data) {
        result=LZString.decompressFromUTF16(data);
	return result;
    }

};

iasylum.compression = iasylum_compression;
"compression.js";
