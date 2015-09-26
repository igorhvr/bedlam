if (!iasylum) {
    var dependence = 'crypto.js depends on iasylum.js';
    console.error(dependence);
    throw dependence;
}

var iasylum_crypto = {
    'generate_sjcl_el_gammal_ecc_c256_keypair': function () {
        var keypair = sjcl.ecc.elGamal.generateKeys(sjcl.ecc.curves['c256'], 10);
        var resultpub = {};
        var resultsec = {};
        resultpub['pub'] = JSON.parse(JSON.stringify(keypair.pub.get()));
        resultsec['sec'] = JSON.parse(JSON.stringify(keypair.sec.get()));
        var finalresultpub = {'type': 'sjcl_el_gammal_ecc_c256_key', 'body': resultpub};
        var finalresultsec = {'type': 'sjcl_el_gammal_ecc_c256_key', 'body': resultsec};
        return JSON.stringify({'publicKey': finalresultpub, 'secretKey': finalresultsec});
    },

    'symmetric_encrypt': function (key, data) {
        return sjcl.encrypt(key, data);
    },

    'symmetric_decrypt': function (key, data) {
        return sjcl.decrypt(key, data);
    },

    'asymmetric_encrypt': function (stringKey, data) {
        var key = JSON.parse(stringKey);

        if (key['type'] == 'sjcl_el_gammal_ecc_c256_key') {
            var buildSJCLKeyObject = function (serializedPublicKey) {
                var curve = sjcl.ecc.curves.c256;

                var points = new sjcl.ecc['point'](
                    curve, new curve.field(sjcl.bn.fromBits(serializedPublicKey.x)),
                    new curve.field(sjcl.bn.fromBits(serializedPublicKey.y)));

                return new sjcl.ecc.elGamal['publicKey'](curve, points);
            };

            var underlying_key = key['body'];

            return sjcl.encrypt(buildSJCLKeyObject(underlying_key['pub']), data);
        } else {
            throw 'unsupported key: ' + key;
        }
    },

    'asymmetric_decrypt': function (stringKey, data) {
        var key = JSON.parse(stringKey);

        if (key['type'] == 'sjcl_el_gammal_ecc_c256_key') {

            var buildSJCLKeyObject = function (serializedPrivateKey) {
                var curve = sjcl.ecc.curves.c256;
                var bigNumber = sjcl.bn.fromBits(serializedPrivateKey);
                return new sjcl.ecc.elGamal['secretKey'](curve, bigNumber);
            };

            var underlying_key = key['body'];

            return sjcl.decrypt(buildSJCLKeyObject(underlying_key['sec']), data);
        } else {
            throw 'unsupported key: ' + key;
        }
    },

    'hmac' : function(stringKey, data) {
        var key = sjcl.codec.utf8String.toBits(stringKey);
        var out = (new sjcl.misc.hmac(key, sjcl.hash.sha256)).mac(data);
        var hmac = sjcl.codec.hex.fromBits(out);
        return hmac;
    }

};

iasylum.crypto = iasylum_crypto;
