// Compiled by ClojureScript 1.10.597 {:target :nodejs}
goog.provide('fipp.clojure');
goog.require('cljs.core');
goog.require('clojure.walk');
goog.require('fipp.visit');
goog.require('fipp.edn');
fipp.clojure.block = (function fipp$clojure$block(nodes){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"nest","nest",-314993663),(2),cljs.core.interpose.call(null,new cljs.core.Keyword(null,"line","line",212345235),nodes)], null);
});
fipp.clojure.list_group = (function fipp$clojure$list_group(var_args){
var args__4795__auto__ = [];
var len__4789__auto___2743 = arguments.length;
var i__4790__auto___2744 = (0);
while(true){
if((i__4790__auto___2744 < len__4789__auto___2743)){
args__4795__auto__.push((arguments[i__4790__auto___2744]));

var G__2745 = (i__4790__auto___2744 + (1));
i__4790__auto___2744 = G__2745;
continue;
} else {
}
break;
}

var argseq__4796__auto__ = ((((0) < args__4795__auto__.length))?(new cljs.core.IndexedSeq(args__4795__auto__.slice((0)),(0),null)):null);
return fipp.clojure.list_group.cljs$core$IFn$_invoke$arity$variadic(argseq__4796__auto__);
});

(fipp.clojure.list_group.cljs$core$IFn$_invoke$arity$variadic = (function (nodes){
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"group","group",582596132),"(",nodes,")"], null);
}));

(fipp.clojure.list_group.cljs$lang$maxFixedArity = (0));

/** @this {Function} */
(fipp.clojure.list_group.cljs$lang$applyTo = (function (seq2742){
var self__4777__auto__ = this;
return self__4777__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq.call(null,seq2742));
}));

fipp.clojure.maybe_a = (function fipp$clojure$maybe_a(pred,xs){
var x = cljs.core.first.call(null,xs);
if(cljs.core.truth_(pred.call(null,x))){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [x,cljs.core.rest.call(null,xs)], null);
} else {
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [null,xs], null);
}
});
fipp.clojure.pretty_cond_clause = (function fipp$clojure$pretty_cond_clause(p,p__2746){
var vec__2747 = p__2746;
var test = cljs.core.nth.call(null,vec__2747,(0),null);
var result = cljs.core.nth.call(null,vec__2747,(1),null);
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"group","group",582596132),fipp.visit.visit.call(null,p,test),new cljs.core.Keyword(null,"line","line",212345235),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"nest","nest",-314993663),(2),fipp.visit.visit.call(null,p,result)], null)], null);
});
fipp.clojure.pretty_case = (function fipp$clojure$pretty_case(p,p__2751){
var vec__2752 = p__2751;
var seq__2753 = cljs.core.seq.call(null,vec__2752);
var first__2754 = cljs.core.first.call(null,seq__2753);
var seq__2753__$1 = cljs.core.next.call(null,seq__2753);
var head = first__2754;
var first__2754__$1 = cljs.core.first.call(null,seq__2753__$1);
var seq__2753__$2 = cljs.core.next.call(null,seq__2753__$1);
var expr = first__2754__$1;
var more = seq__2753__$2;
var clauses = cljs.core.partition.call(null,(2),more);
var default$ = ((cljs.core.odd_QMARK_.call(null,cljs.core.count.call(null,more)))?cljs.core.last.call(null,more):null);
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,expr),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.concat.call(null,cljs.core.map.call(null,(function (p1__2750_SHARP_){
return fipp.clojure.pretty_cond_clause.call(null,p,p1__2750_SHARP_);
}),clauses),(cljs.core.truth_(default$)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,default$)], null):null))));
});
fipp.clojure.pretty_cond = (function fipp$clojure$pretty_cond(p,p__2756){
var vec__2757 = p__2756;
var seq__2758 = cljs.core.seq.call(null,vec__2757);
var first__2759 = cljs.core.first.call(null,seq__2758);
var seq__2758__$1 = cljs.core.next.call(null,seq__2758);
var head = first__2759;
var more = seq__2758__$1;
var clauses = cljs.core.partition.call(null,(2),more);
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__2755_SHARP_){
return fipp.clojure.pretty_cond_clause.call(null,p,p1__2755_SHARP_);
}),clauses)));
});
fipp.clojure.pretty_condp = (function fipp$clojure$pretty_condp(p,p__2761){
var vec__2762 = p__2761;
var seq__2763 = cljs.core.seq.call(null,vec__2762);
var first__2764 = cljs.core.first.call(null,seq__2763);
var seq__2763__$1 = cljs.core.next.call(null,seq__2763);
var head = first__2764;
var first__2764__$1 = cljs.core.first.call(null,seq__2763__$1);
var seq__2763__$2 = cljs.core.next.call(null,seq__2763__$1);
var pred = first__2764__$1;
var first__2764__$2 = cljs.core.first.call(null,seq__2763__$2);
var seq__2763__$3 = cljs.core.next.call(null,seq__2763__$2);
var expr = first__2764__$2;
var more = seq__2763__$3;
var clauses = cljs.core.partition.call(null,(2),more);
var default$ = ((cljs.core.odd_QMARK_.call(null,cljs.core.count.call(null,more)))?cljs.core.last.call(null,more):null);
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,pred)," ",fipp.visit.visit.call(null,p,expr),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.concat.call(null,cljs.core.map.call(null,(function (p1__2760_SHARP_){
return fipp.clojure.pretty_cond_clause.call(null,p,p1__2760_SHARP_);
}),clauses),(cljs.core.truth_(default$)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,default$)], null):null))));
});
fipp.clojure.pretty_arrow = (function fipp$clojure$pretty_arrow(p,p__2766){
var vec__2767 = p__2766;
var seq__2768 = cljs.core.seq.call(null,vec__2767);
var first__2769 = cljs.core.first.call(null,seq__2768);
var seq__2768__$1 = cljs.core.next.call(null,seq__2768);
var head = first__2769;
var stmts = seq__2768__$1;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),cljs.core.interpose.call(null,new cljs.core.Keyword(null,"line","line",212345235),cljs.core.map.call(null,(function (p1__2765_SHARP_){
return fipp.visit.visit.call(null,p,p1__2765_SHARP_);
}),stmts))], null));
});
fipp.clojure.pretty_if = (function fipp$clojure$pretty_if(p,p__2771){
var vec__2772 = p__2771;
var seq__2773 = cljs.core.seq.call(null,vec__2772);
var first__2774 = cljs.core.first.call(null,seq__2773);
var seq__2773__$1 = cljs.core.next.call(null,seq__2773);
var head = first__2774;
var first__2774__$1 = cljs.core.first.call(null,seq__2773__$1);
var seq__2773__$2 = cljs.core.next.call(null,seq__2773__$1);
var test = first__2774__$1;
var more = seq__2773__$2;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,test),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__2770_SHARP_){
return fipp.visit.visit.call(null,p,p1__2770_SHARP_);
}),more)));
});
fipp.clojure.pretty_method = (function fipp$clojure$pretty_method(p,p__2776){
var vec__2777 = p__2776;
var seq__2778 = cljs.core.seq.call(null,vec__2777);
var first__2779 = cljs.core.first.call(null,seq__2778);
var seq__2778__$1 = cljs.core.next.call(null,seq__2778);
var params = first__2779;
var body = seq__2778__$1;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,params),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__2775_SHARP_){
return fipp.visit.visit.call(null,p,p1__2775_SHARP_);
}),body)));
});
fipp.clojure.pretty_defn = (function fipp$clojure$pretty_defn(p,p__2782){
var vec__2783 = p__2782;
var seq__2784 = cljs.core.seq.call(null,vec__2783);
var first__2785 = cljs.core.first.call(null,seq__2784);
var seq__2784__$1 = cljs.core.next.call(null,seq__2784);
var head = first__2785;
var first__2785__$1 = cljs.core.first.call(null,seq__2784__$1);
var seq__2784__$2 = cljs.core.next.call(null,seq__2784__$1);
var fn_name = first__2785__$1;
var more = seq__2784__$2;
var vec__2786 = fipp.clojure.maybe_a.call(null,cljs.core.string_QMARK_,more);
var docstring = cljs.core.nth.call(null,vec__2786,(0),null);
var more__$1 = cljs.core.nth.call(null,vec__2786,(1),null);
var vec__2789 = fipp.clojure.maybe_a.call(null,cljs.core.map_QMARK_,more__$1);
var attr_map = cljs.core.nth.call(null,vec__2789,(0),null);
var more__$2 = cljs.core.nth.call(null,vec__2789,(1),null);
var vec__2792 = fipp.clojure.maybe_a.call(null,cljs.core.vector_QMARK_,more__$2);
var params = cljs.core.nth.call(null,vec__2792,(0),null);
var body = cljs.core.nth.call(null,vec__2792,(1),null);
var params_on_first_line_QMARK_ = (function (){var and__4174__auto__ = params;
if(cljs.core.truth_(and__4174__auto__)){
return (((docstring == null)) && ((attr_map == null)));
} else {
return and__4174__auto__;
}
})();
var params_after_attr_map_QMARK_ = (function (){var and__4174__auto__ = params;
if(cljs.core.truth_(and__4174__auto__)){
return cljs.core.not.call(null,params_on_first_line_QMARK_);
} else {
return and__4174__auto__;
}
})();
return fipp.clojure.list_group.call(null,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,fn_name)], null),(cljs.core.truth_(params_on_first_line_QMARK_)?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [" ",fipp.visit.visit.call(null,p,params)], null):null)),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.concat.call(null,(cljs.core.truth_(docstring)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,docstring)], null):null),(cljs.core.truth_(attr_map)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,attr_map)], null):null),(cljs.core.truth_(params_after_attr_map_QMARK_)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,params)], null):null),(cljs.core.truth_(body)?cljs.core.map.call(null,(function (p1__2780_SHARP_){
return fipp.visit.visit.call(null,p,p1__2780_SHARP_);
}),body):cljs.core.map.call(null,(function (p1__2781_SHARP_){
return fipp.clojure.pretty_method.call(null,p,p1__2781_SHARP_);
}),more__$2)))));
});
fipp.clojure.pretty_fn = (function fipp$clojure$pretty_fn(p,p__2797){
var vec__2798 = p__2797;
var seq__2799 = cljs.core.seq.call(null,vec__2798);
var first__2800 = cljs.core.first.call(null,seq__2799);
var seq__2799__$1 = cljs.core.next.call(null,seq__2799);
var head = first__2800;
var more = seq__2799__$1;
var vec__2801 = fipp.clojure.maybe_a.call(null,cljs.core.symbol_QMARK_,more);
var fn_name = cljs.core.nth.call(null,vec__2801,(0),null);
var more__$1 = cljs.core.nth.call(null,vec__2801,(1),null);
var vec__2804 = fipp.clojure.maybe_a.call(null,cljs.core.vector_QMARK_,more__$1);
var params = cljs.core.nth.call(null,vec__2804,(0),null);
var body = cljs.core.nth.call(null,vec__2804,(1),null);
return fipp.clojure.list_group.call(null,cljs.core.concat.call(null,new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,head)], null),(cljs.core.truth_(fn_name)?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [" ",fipp.visit.visit.call(null,p,fn_name)], null):null),(cljs.core.truth_(params)?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [" ",fipp.visit.visit.call(null,p,params)], null):null)),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,(cljs.core.truth_(body)?cljs.core.map.call(null,(function (p1__2795_SHARP_){
return fipp.visit.visit.call(null,p,p1__2795_SHARP_);
}),body):cljs.core.map.call(null,(function (p1__2796_SHARP_){
return fipp.clojure.pretty_method.call(null,p,p1__2796_SHARP_);
}),more__$1))));
});
fipp.clojure.pretty_fn_STAR_ = (function fipp$clojure$pretty_fn_STAR_(p,p__2810){
var vec__2811 = p__2810;
var _ = cljs.core.nth.call(null,vec__2811,(0),null);
var params = cljs.core.nth.call(null,vec__2811,(1),null);
var body = cljs.core.nth.call(null,vec__2811,(2),null);
var form = vec__2811;
if(((cljs.core.vector_QMARK_.call(null,params)) && (cljs.core.seq_QMARK_.call(null,body)))){
var vec__2814 = cljs.core.split_with.call(null,(function (p1__2807_SHARP_){
return cljs.core.not_EQ_.call(null,p1__2807_SHARP_,new cljs.core.Symbol(null,"&","&",-2144855648,null));
}),params);
var inits = cljs.core.nth.call(null,vec__2814,(0),null);
var rests = cljs.core.nth.call(null,vec__2814,(1),null);
var params_STAR_ = cljs.core.merge.call(null,((cljs.core._EQ_.call(null,cljs.core.count.call(null,inits),(1)))?cljs.core.PersistentArrayMap.createAsIfByAssoc([cljs.core.first.call(null,inits),new cljs.core.Symbol(null,"%","%",-950237169,null)]):cljs.core.zipmap.call(null,inits,cljs.core.map.call(null,(function (p1__2808_SHARP_){
return cljs.core.symbol.call(null,["%",cljs.core.str.cljs$core$IFn$_invoke$arity$1((p1__2808_SHARP_ + (1)))].join(''));
}),cljs.core.range.call(null)))),((cljs.core.seq.call(null,rests))?cljs.core.PersistentArrayMap.createAsIfByAssoc([cljs.core.second.call(null,rests),new cljs.core.Symbol(null,"%&","%&",-728707069,null)]):null));
var body_STAR_ = clojure.walk.prewalk_replace.call(null,params_STAR_,body);
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"group","group",582596132),"#(",new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),(2),cljs.core.interpose.call(null,new cljs.core.Keyword(null,"line","line",212345235),cljs.core.map.call(null,(function (p1__2809_SHARP_){
return fipp.visit.visit.call(null,p,p1__2809_SHARP_);
}),body_STAR_))], null),")"], null);
} else {
return fipp.clojure.pretty_fn.call(null,p,form);
}
});
fipp.clojure.pretty_libspec = (function fipp$clojure$pretty_libspec(p,p__2818){
var vec__2819 = p__2818;
var seq__2820 = cljs.core.seq.call(null,vec__2819);
var first__2821 = cljs.core.first.call(null,seq__2820);
var seq__2820__$1 = cljs.core.next.call(null,seq__2820);
var head = first__2821;
var clauses = seq__2820__$1;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),cljs.core.interpose.call(null,new cljs.core.Keyword(null,"line","line",212345235),cljs.core.map.call(null,(function (p1__2817_SHARP_){
return fipp.visit.visit.call(null,p,p1__2817_SHARP_);
}),clauses))], null));
});
fipp.clojure.pretty_ns = (function fipp$clojure$pretty_ns(p,p__2823){
var vec__2824 = p__2823;
var seq__2825 = cljs.core.seq.call(null,vec__2824);
var first__2826 = cljs.core.first.call(null,seq__2825);
var seq__2825__$1 = cljs.core.next.call(null,seq__2825);
var head = first__2826;
var first__2826__$1 = cljs.core.first.call(null,seq__2825__$1);
var seq__2825__$2 = cljs.core.next.call(null,seq__2825__$1);
var ns_sym = first__2826__$1;
var more = seq__2825__$2;
var vec__2827 = fipp.clojure.maybe_a.call(null,cljs.core.string_QMARK_,more);
var docstring = cljs.core.nth.call(null,vec__2827,(0),null);
var more__$1 = cljs.core.nth.call(null,vec__2827,(1),null);
var vec__2830 = fipp.clojure.maybe_a.call(null,cljs.core.map_QMARK_,more__$1);
var attr_map = cljs.core.nth.call(null,vec__2830,(0),null);
var specs = cljs.core.nth.call(null,vec__2830,(1),null);
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,ns_sym),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.concat.call(null,(cljs.core.truth_(docstring)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,docstring)], null):null),(cljs.core.truth_(attr_map)?new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [fipp.visit.visit.call(null,p,attr_map)], null):null),cljs.core.map.call(null,(function (p1__2822_SHARP_){
return fipp.clojure.pretty_libspec.call(null,p,p1__2822_SHARP_);
}),specs))));
});
fipp.clojure.pretty_quote = (function fipp$clojure$pretty_quote(p,p__2833){
var vec__2834 = p__2833;
var macro = cljs.core.nth.call(null,vec__2834,(0),null);
var arg = cljs.core.nth.call(null,vec__2834,(1),null);
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span","span",1394872991),(function (){var G__2837 = cljs.core.keyword.call(null,cljs.core.name.call(null,macro));
var G__2837__$1 = (((G__2837 instanceof cljs.core.Keyword))?G__2837.fqn:null);
switch (G__2837__$1) {
case "deref":
return "@";

break;
case "quote":
return "'";

break;
case "unquote":
return "~";

break;
case "var":
return "#'";

break;
default:
throw (new Error(["No matching clause: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(G__2837__$1)].join('')));

}
})(),fipp.visit.visit.call(null,p,arg)], null);
});
fipp.clojure.pretty_bindings = (function fipp$clojure$pretty_bindings(p,bvec){
var kvps = (function (){var iter__4582__auto__ = (function fipp$clojure$pretty_bindings_$_iter__2839(s__2840){
return (new cljs.core.LazySeq(null,(function (){
var s__2840__$1 = s__2840;
while(true){
var temp__5720__auto__ = cljs.core.seq.call(null,s__2840__$1);
if(temp__5720__auto__){
var s__2840__$2 = temp__5720__auto__;
if(cljs.core.chunked_seq_QMARK_.call(null,s__2840__$2)){
var c__4580__auto__ = cljs.core.chunk_first.call(null,s__2840__$2);
var size__4581__auto__ = cljs.core.count.call(null,c__4580__auto__);
var b__2842 = cljs.core.chunk_buffer.call(null,size__4581__auto__);
if((function (){var i__2841 = (0);
while(true){
if((i__2841 < size__4581__auto__)){
var vec__2843 = cljs.core._nth.call(null,c__4580__auto__,i__2841);
var k = cljs.core.nth.call(null,vec__2843,(0),null);
var v = cljs.core.nth.call(null,vec__2843,(1),null);
cljs.core.chunk_append.call(null,b__2842,new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span","span",1394872991),fipp.visit.visit.call(null,p,k)," ",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),fipp.visit.visit.call(null,p,v)], null)], null));

var G__2849 = (i__2841 + (1));
i__2841 = G__2849;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons.call(null,cljs.core.chunk.call(null,b__2842),fipp$clojure$pretty_bindings_$_iter__2839.call(null,cljs.core.chunk_rest.call(null,s__2840__$2)));
} else {
return cljs.core.chunk_cons.call(null,cljs.core.chunk.call(null,b__2842),null);
}
} else {
var vec__2846 = cljs.core.first.call(null,s__2840__$2);
var k = cljs.core.nth.call(null,vec__2846,(0),null);
var v = cljs.core.nth.call(null,vec__2846,(1),null);
return cljs.core.cons.call(null,new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"span","span",1394872991),fipp.visit.visit.call(null,p,k)," ",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),fipp.visit.visit.call(null,p,v)], null)], null),fipp$clojure$pretty_bindings_$_iter__2839.call(null,cljs.core.rest.call(null,s__2840__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4582__auto__.call(null,cljs.core.partition.call(null,(2),bvec));
})();
return new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"group","group",582596132),"[",new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"align","align",1964212802),cljs.core.interpose.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"line","line",212345235),", "], null),kvps)], null),"]"], null);
});
fipp.clojure.pretty_let = (function fipp$clojure$pretty_let(p,p__2851){
var vec__2852 = p__2851;
var seq__2853 = cljs.core.seq.call(null,vec__2852);
var first__2854 = cljs.core.first.call(null,seq__2853);
var seq__2853__$1 = cljs.core.next.call(null,seq__2853);
var head = first__2854;
var first__2854__$1 = cljs.core.first.call(null,seq__2853__$1);
var seq__2853__$2 = cljs.core.next.call(null,seq__2853__$1);
var bvec = first__2854__$1;
var body = seq__2853__$2;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.clojure.pretty_bindings.call(null,p,bvec),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__2850_SHARP_){
return fipp.visit.visit.call(null,p,p1__2850_SHARP_);
}),body)));
});
fipp.clojure.pretty_impls = (function fipp$clojure$pretty_impls(p,opts_PLUS_specs){
return fipp.clojure.block.call(null,cljs.core.map.call(null,(function (p1__2855_SHARP_){
return fipp.visit.visit.call(null,p,p1__2855_SHARP_);
}),opts_PLUS_specs));
});
fipp.clojure.pretty_type = (function fipp$clojure$pretty_type(p,p__2856){
var vec__2857 = p__2856;
var seq__2858 = cljs.core.seq.call(null,vec__2857);
var first__2859 = cljs.core.first.call(null,seq__2858);
var seq__2858__$1 = cljs.core.next.call(null,seq__2858);
var head = first__2859;
var first__2859__$1 = cljs.core.first.call(null,seq__2858__$1);
var seq__2858__$2 = cljs.core.next.call(null,seq__2858__$1);
var fields = first__2859__$1;
var opts_PLUS_specs = seq__2858__$2;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head)," ",fipp.visit.visit.call(null,p,fields),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.pretty_impls.call(null,p,opts_PLUS_specs));
});
fipp.clojure.pretty_reify = (function fipp$clojure$pretty_reify(p,p__2860){
var vec__2861 = p__2860;
var seq__2862 = cljs.core.seq.call(null,vec__2861);
var first__2863 = cljs.core.first.call(null,seq__2862);
var seq__2862__$1 = cljs.core.next.call(null,seq__2862);
var head = first__2863;
var opts_PLUS_specs = seq__2862__$1;
return fipp.clojure.list_group.call(null,fipp.visit.visit.call(null,p,head),new cljs.core.Keyword(null,"line","line",212345235),fipp.clojure.pretty_impls.call(null,p,opts_PLUS_specs));
});
fipp.clojure.build_symbol_map = (function fipp$clojure$build_symbol_map(dispatch){
return cljs.core.into.call(null,cljs.core.PersistentArrayMap.EMPTY,(function (){var iter__4582__auto__ = (function fipp$clojure$build_symbol_map_$_iter__2864(s__2865){
return (new cljs.core.LazySeq(null,(function (){
var s__2865__$1 = s__2865;
while(true){
var temp__5720__auto__ = cljs.core.seq.call(null,s__2865__$1);
if(temp__5720__auto__){
var xs__6277__auto__ = temp__5720__auto__;
var vec__2872 = cljs.core.first.call(null,xs__6277__auto__);
var pretty_fn = cljs.core.nth.call(null,vec__2872,(0),null);
var syms = cljs.core.nth.call(null,vec__2872,(1),null);
var iterys__4578__auto__ = ((function (s__2865__$1,vec__2872,pretty_fn,syms,xs__6277__auto__,temp__5720__auto__){
return (function fipp$clojure$build_symbol_map_$_iter__2864_$_iter__2866(s__2867){
return (new cljs.core.LazySeq(null,((function (s__2865__$1,vec__2872,pretty_fn,syms,xs__6277__auto__,temp__5720__auto__){
return (function (){
var s__2867__$1 = s__2867;
while(true){
var temp__5720__auto____$1 = cljs.core.seq.call(null,s__2867__$1);
if(temp__5720__auto____$1){
var xs__6277__auto____$1 = temp__5720__auto____$1;
var sym = cljs.core.first.call(null,xs__6277__auto____$1);
var iterys__4578__auto__ = ((function (s__2867__$1,s__2865__$1,sym,xs__6277__auto____$1,temp__5720__auto____$1,vec__2872,pretty_fn,syms,xs__6277__auto__,temp__5720__auto__){
return (function fipp$clojure$build_symbol_map_$_iter__2864_$_iter__2866_$_iter__2868(s__2869){
return (new cljs.core.LazySeq(null,((function (s__2867__$1,s__2865__$1,sym,xs__6277__auto____$1,temp__5720__auto____$1,vec__2872,pretty_fn,syms,xs__6277__auto__,temp__5720__auto__){
return (function (){
var s__2869__$1 = s__2869;
while(true){
var temp__5720__auto____$2 = cljs.core.seq.call(null,s__2869__$1);
if(temp__5720__auto____$2){
var s__2869__$2 = temp__5720__auto____$2;
if(cljs.core.chunked_seq_QMARK_.call(null,s__2869__$2)){
var c__4580__auto__ = cljs.core.chunk_first.call(null,s__2869__$2);
var size__4581__auto__ = cljs.core.count.call(null,c__4580__auto__);
var b__2871 = cljs.core.chunk_buffer.call(null,size__4581__auto__);
if((function (){var i__2870 = (0);
while(true){
if((i__2870 < size__4581__auto__)){
var sym__$1 = cljs.core._nth.call(null,c__4580__auto__,i__2870);
cljs.core.chunk_append.call(null,b__2871,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [sym__$1,pretty_fn], null));

var G__2875 = (i__2870 + (1));
i__2870 = G__2875;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons.call(null,cljs.core.chunk.call(null,b__2871),fipp$clojure$build_symbol_map_$_iter__2864_$_iter__2866_$_iter__2868.call(null,cljs.core.chunk_rest.call(null,s__2869__$2)));
} else {
return cljs.core.chunk_cons.call(null,cljs.core.chunk.call(null,b__2871),null);
}
} else {
var sym__$1 = cljs.core.first.call(null,s__2869__$2);
return cljs.core.cons.call(null,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [sym__$1,pretty_fn], null),fipp$clojure$build_symbol_map_$_iter__2864_$_iter__2866_$_iter__2868.call(null,cljs.core.rest.call(null,s__2869__$2)));
}
} else {
return null;
}
break;
}
});})(s__2867__$1,s__2865__$1,sym,xs__6277__auto____$1,temp__5720__auto____$1,vec__2872,pretty_fn,syms,xs__6277__auto__,temp__5720__auto__))
,null,null));
});})(s__2867__$1,s__2865__$1,sym,xs__6277__auto____$1,temp__5720__auto____$1,vec__2872,pretty_fn,syms,xs__6277__auto__,temp__5720__auto__))
;
var fs__4579__auto__ = cljs.core.seq.call(null,iterys__4578__auto__.call(null,cljs.core.cons.call(null,sym,((cljs.core.special_symbol_QMARK_.call(null,sym))?null:new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.symbol.call(null,"clojure.core",cljs.core.name.call(null,sym)),cljs.core.symbol.call(null,"cljs.core",cljs.core.name.call(null,sym))], null)))));
if(fs__4579__auto__){
return cljs.core.concat.call(null,fs__4579__auto__,fipp$clojure$build_symbol_map_$_iter__2864_$_iter__2866.call(null,cljs.core.rest.call(null,s__2867__$1)));
} else {
var G__2876 = cljs.core.rest.call(null,s__2867__$1);
s__2867__$1 = G__2876;
continue;
}
} else {
return null;
}
break;
}
});})(s__2865__$1,vec__2872,pretty_fn,syms,xs__6277__auto__,temp__5720__auto__))
,null,null));
});})(s__2865__$1,vec__2872,pretty_fn,syms,xs__6277__auto__,temp__5720__auto__))
;
var fs__4579__auto__ = cljs.core.seq.call(null,iterys__4578__auto__.call(null,syms));
if(fs__4579__auto__){
return cljs.core.concat.call(null,fs__4579__auto__,fipp$clojure$build_symbol_map_$_iter__2864.call(null,cljs.core.rest.call(null,s__2865__$1)));
} else {
var G__2877 = cljs.core.rest.call(null,s__2865__$1);
s__2865__$1 = G__2877;
continue;
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4582__auto__.call(null,dispatch);
})());
});
fipp.clojure.default_symbols = fipp.clojure.build_symbol_map.call(null,cljs.core.PersistentHashMap.fromArrays([fipp.clojure.pretty_fn_STAR_,fipp.clojure.pretty_condp,fipp.clojure.pretty_quote,fipp.clojure.pretty_cond,fipp.clojure.pretty_fn,fipp.clojure.pretty_arrow,fipp.clojure.pretty_reify,fipp.clojure.pretty_let,fipp.clojure.pretty_type,fipp.clojure.pretty_if,fipp.clojure.pretty_defn,fipp.clojure.pretty_ns,fipp.clojure.pretty_case],[new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"fn*","fn*",-752876845,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"condp","condp",1054325175,null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"deref","deref",1494944732,null),new cljs.core.Symbol(null,"quote","quote",1377916282,null),new cljs.core.Symbol(null,"unquote","unquote",-1004694737,null),new cljs.core.Symbol(null,"var","var",870848730,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"cond","cond",1606708055,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"fn","fn",465265323,null)], null),new cljs.core.PersistentVector(null, 9, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,".",".",1975675962,null),new cljs.core.Symbol(null,"..","..",-300507420,null),new cljs.core.Symbol(null,"->","->",-2139605430,null),new cljs.core.Symbol(null,"->>","->>",-1874332161,null),new cljs.core.Symbol(null,"and","and",668631710,null),new cljs.core.Symbol(null,"doto","doto",1252536074,null),new cljs.core.Symbol(null,"or","or",1876275696,null),new cljs.core.Symbol(null,"some->","some->",-1011172200,null),new cljs.core.Symbol(null,"some->>","some->>",-1499987794,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"reify","reify",1885539699,null)], null),new cljs.core.PersistentVector(null, 16, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"binding","binding",-2114503176,null),new cljs.core.Symbol(null,"doseq","doseq",221164135,null),new cljs.core.Symbol(null,"dotimes","dotimes",-818708397,null),new cljs.core.Symbol(null,"for","for",316745208,null),new cljs.core.Symbol(null,"if-let","if-let",1803593690,null),new cljs.core.Symbol(null,"if-some","if-some",1960677609,null),new cljs.core.Symbol(null,"let","let",358118826,null),new cljs.core.Symbol(null,"let*","let*",1920721458,null),new cljs.core.Symbol(null,"loop","loop",1244978678,null),new cljs.core.Symbol(null,"loop*","loop*",615029416,null),new cljs.core.Symbol(null,"when-first","when-first",821699168,null),new cljs.core.Symbol(null,"when-let","when-let",-1383043480,null),new cljs.core.Symbol(null,"when-some","when-some",1700415903,null),new cljs.core.Symbol(null,"with-local-vars","with-local-vars",837642072,null),new cljs.core.Symbol(null,"with-open","with-open",172119667,null),new cljs.core.Symbol(null,"with-redefs","with-redefs",-1143728263,null)], null),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"deftype","deftype",1980826088,null),new cljs.core.Symbol(null,"defrecord","defrecord",273038109,null)], null),new cljs.core.PersistentVector(null, 6, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"def","def",597100991,null),new cljs.core.Symbol(null,"defonce","defonce",-1681484013,null),new cljs.core.Symbol(null,"if","if",1181717262,null),new cljs.core.Symbol(null,"if-not","if-not",-265415609,null),new cljs.core.Symbol(null,"when","when",1064114221,null),new cljs.core.Symbol(null,"when-not","when-not",-1223136340,null)], null),new cljs.core.PersistentVector(null, 4, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"defmacro","defmacro",2054157304,null),new cljs.core.Symbol(null,"defmulti","defmulti",1936112154,null),new cljs.core.Symbol(null,"defn","defn",-126010802,null),new cljs.core.Symbol(null,"defn-","defn-",1097765044,null)], null),new cljs.core.PersistentVector(null, 1, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"ns","ns",2082130287,null)], null),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Symbol(null,"case","case",-1510733573,null),new cljs.core.Symbol(null,"cond->","cond->",561741875,null),new cljs.core.Symbol(null,"cond->>","cond->>",348844960,null)], null)]));
fipp.clojure.pprint = (function fipp$clojure$pprint(var_args){
var G__2879 = arguments.length;
switch (G__2879) {
case 1:
return fipp.clojure.pprint.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return fipp.clojure.pprint.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(fipp.clojure.pprint.cljs$core$IFn$_invoke$arity$1 = (function (x){
return fipp.clojure.pprint.call(null,x,cljs.core.PersistentArrayMap.EMPTY);
}));

(fipp.clojure.pprint.cljs$core$IFn$_invoke$arity$2 = (function (x,options){
return fipp.edn.pprint.call(null,x,cljs.core.merge.call(null,new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"symbols","symbols",1211743),fipp.clojure.default_symbols], null),options));
}));

(fipp.clojure.pprint.cljs$lang$maxFixedArity = 2);


//# sourceMappingURL=clojure.js.map
