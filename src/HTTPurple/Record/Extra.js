
export const pickFn = function(ks, r) {
    var copy = {};
    for(var i = 0; i < ks.length; i++) {
        copy[ks[i]] = r[ks[i]];
    }
    return copy;
  };
