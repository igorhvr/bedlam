var iasylum;
var LZString;

if (!iasylum || !LZString) {
    var dependence = 'compression.js depends on iasylum.js from bedlam and LZString from the lz-string.js library.';
    console.error(dependence);
    throw dependence;
}

var iasylum_compression = {
    'lzw_compress': function (data) {
        console.debug("About to compress:", data);
	//        result=LZString.compressToBase64(data);
	result=LZString.compressToUTF16(data);
        console.debug("Sucess. Result is: ",result);
	return result;
    },

    'lzw_decompress': function (data) {
        console.debug("About to decompress:", data);
	//        result=LZString.decompressFromBase64(data);
        result=LZString.decompressFromUTF16(data);
        console.debug("Sucess. Result is: ",result);
	return result;
    }

};

iasylum.compression = iasylum_compression;
