// Compiled by ClojureScript 1.10.597 {:target :nodejs}
goog.provide('allpa.core');
goog.require('cljs.core');
allpa.core.power = (function allpa$core$power(b,e){
if((e < (0))){
return (0);
} else {
return Math.round(Math.floor(Math.pow(b,e)));
}
});
allpa.core.p2 = (function allpa$core$p2(n){
return cljs.core.partial.call(null,allpa.core.power,(2));
});

//# sourceMappingURL=core.js.map
