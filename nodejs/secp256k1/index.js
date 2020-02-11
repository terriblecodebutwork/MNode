const elliptic = require('elliptic')
const BN = require('bn.js')
// const hash = require('hash.js')

let f = (priv, msg, k) => {
    let msghash = msg

    let curve = new elliptic.ec("secp256k1");

    let keys = curve.keyFromPrivate(priv);

    let sign = keys.sign(msghash, {
        k: function (iter) {
            return new BN(k, 10);
        }
    });

    return sign.toDER('hex');
}

module.exports = f
