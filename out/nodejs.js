// Compiled by ClojureScript 1.10.597 {:target :nodejs}
goog.provide('cljs.nodejs');
goog.require('cljs.core');
cljs.nodejs.require = require;
cljs.nodejs.process = process;
cljs.nodejs.enable_util_print_BANG_ = (function cljs$nodejs$enable_util_print_BANG_(){
(cljs.core._STAR_print_newline_STAR_ = false);

cljs.core.set_print_fn_BANG_.call(null,(function() { 
var G__3116__delegate = function (args){
return console.log.apply(console,cljs.core.into_array.call(null,args));
};
var G__3116 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__3117__i = 0, G__3117__a = new Array(arguments.length -  0);
while (G__3117__i < G__3117__a.length) {G__3117__a[G__3117__i] = arguments[G__3117__i + 0]; ++G__3117__i;}
  args = new cljs.core.IndexedSeq(G__3117__a,0,null);
} 
return G__3116__delegate.call(this,args);};
G__3116.cljs$lang$maxFixedArity = 0;
G__3116.cljs$lang$applyTo = (function (arglist__3118){
var args = cljs.core.seq(arglist__3118);
return G__3116__delegate(args);
});
G__3116.cljs$core$IFn$_invoke$arity$variadic = G__3116__delegate;
return G__3116;
})()
);

cljs.core.set_print_err_fn_BANG_.call(null,(function() { 
var G__3119__delegate = function (args){
return console.error.apply(console,cljs.core.into_array.call(null,args));
};
var G__3119 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__3120__i = 0, G__3120__a = new Array(arguments.length -  0);
while (G__3120__i < G__3120__a.length) {G__3120__a[G__3120__i] = arguments[G__3120__i + 0]; ++G__3120__i;}
  args = new cljs.core.IndexedSeq(G__3120__a,0,null);
} 
return G__3119__delegate.call(this,args);};
G__3119.cljs$lang$maxFixedArity = 0;
G__3119.cljs$lang$applyTo = (function (arglist__3121){
var args = cljs.core.seq(arglist__3121);
return G__3119__delegate(args);
});
G__3119.cljs$core$IFn$_invoke$arity$variadic = G__3119__delegate;
return G__3119;
})()
);

return null;
});

//# sourceMappingURL=nodejs.js.map
