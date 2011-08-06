<<<<<<< HEAD
var BiwaScheme=BiwaScheme||{};BiwaScheme.Version="0.5.7";BiwaScheme.GitCommit="0e4cbc80ed4abcc327a71d8f98300f5b60d2d304";var Prototype={Version:"1.6.0.3",Browser:{IE:!!(window.attachEvent&&navigator.userAgent.indexOf("Opera")===-1),Opera:navigator.userAgent.indexOf("Opera")>-1,WebKit:navigator.userAgent.indexOf("AppleWebKit/")>-1,Gecko:navigator.userAgent.indexOf("Gecko")>-1&&navigator.userAgent.indexOf("KHTML")===-1,MobileSafari:!!navigator.userAgent.match(/Apple.*Mobile.*Safari/)},BrowserFeatures:{XPath:!!document.evaluate,SelectorsAPI:!!document.querySelector,ElementExtensions:!!window.HTMLElement,SpecificElementExtensions:document.createElement("div")["__proto__"]&&document.createElement("div")["__proto__"]!==document.createElement("form")["__proto__"]},ScriptFragment:"<script[^>]*>([\\S\\s]*?)<\/script>",JSONFilter:/^\/\*-secure-([\s\S]*)\*\/\s*$/,emptyFunction:function(){},K:function(a){return a}};if(Prototype.Browser.MobileSafari){Prototype.BrowserFeatures.SpecificElementExtensions=false}var Class={create:function(){var f=null,e=$A(arguments);if(Object.isFunction(e[0])){f=e.shift()}function a(){this.initialize.apply(this,arguments)}Object.extend(a,Class.Methods);a.superclass=f;a.subclasses=[];if(f){var c=function(){};c.prototype=f.prototype;a.prototype=new c;f.subclasses.push(a)}for(var d=0;d<e.length;d++){a.addMethods(e[d])}if(!a.prototype.initialize){a.prototype.initialize=Prototype.emptyFunction}a.prototype.constructor=a;return a}};Class.Methods={addMethods:function(h){var d=this.superclass&&this.superclass.prototype;var c=Object.keys(h);if(!Object.keys({toString:true}).length){c.push("toString","valueOf")}for(var a=0,e=c.length;a<e;a++){var g=c[a],f=h[g];if(d&&Object.isFunction(f)&&f.argumentNames().first()=="$super"){var j=f;f=(function(k){return function(){return d[k].apply(this,arguments)}})(g).wrap(j);f.valueOf=j.valueOf.bind(j);f.toString=j.toString.bind(j)}this.prototype[g]=f}return this}};var Abstract={};Object.extend=function(a,d){for(var c in d){a[c]=d[c]}return a};Object.extend(Object,{inspect:function(a){try{if(Object.isUndefined(a)){return"undefined"}if(a===null){return"null"}return a.inspect?a.inspect():String(a)}catch(c){if(c instanceof RangeError){return"..."}throw c}},toJSON:function(a){var d=typeof a;switch(d){case"undefined":case"function":case"unknown":return;case"boolean":return a.toString()}if(a===null){return"null"}if(a.toJSON){return a.toJSON()}if(Object.isElement(a)){return}var c=[];for(var f in a){var e=Object.toJSON(a[f]);if(!Object.isUndefined(e)){c.push(f.toJSON()+": "+e)}}return"{"+c.join(", ")+"}"},toQueryString:function(a){return $H(a).toQueryString()},toHTML:function(a){return a&&a.toHTML?a.toHTML():String.interpret(a)},keys:function(a){var c=[];for(var d in a){c.push(d)}return c},values:function(c){var a=[];for(var d in c){a.push(c[d])}return a},clone:function(a){return Object.extend({},a)},isElement:function(a){return !!(a&&a.nodeType==1)},isArray:function(a){return a!=null&&typeof a=="object"&&"splice" in a&&"join" in a},isHash:function(a){return a instanceof Hash},isFunction:function(a){return typeof a=="function"},isString:function(a){return typeof a=="string"},isNumber:function(a){return typeof a=="number"},isUndefined:function(a){return typeof a=="undefined"}});Object.extend(Function.prototype,{argumentNames:function(){var a=this.toString().match(/^[\s\(]*function[^(]*\(([^\)]*)\)/)[1].replace(/\s+/g,"").split(",");return a.length==1&&!a[0]?[]:a},bind:function(){if(arguments.length<2&&Object.isUndefined(arguments[0])){return this}var a=this,d=$A(arguments),c=d.shift();return function(){return a.apply(c,d.concat($A(arguments)))}},bindAsEventListener:function(){var a=this,d=$A(arguments),c=d.shift();return function(e){return a.apply(c,[e||window.event].concat(d))}},curry:function(){if(!arguments.length){return this}var a=this,c=$A(arguments);return function(){return a.apply(this,c.concat($A(arguments)))}},delay:function(){var a=this,c=$A(arguments),d=c.shift()*1000;return window.setTimeout(function(){return a.apply(a,c)},d)},defer:function(){var a=[0.01].concat($A(arguments));return this.delay.apply(this,a)},wrap:function(c){var a=this;return function(){return c.apply(this,[a.bind(this)].concat($A(arguments)))}},methodize:function(){if(this._methodized){return this._methodized}var a=this;return this._methodized=function(){return a.apply(null,[this].concat($A(arguments)))}}});Date.prototype.toJSON=function(){return'"'+this.getUTCFullYear()+"-"+(this.getUTCMonth()+1).toPaddedString(2)+"-"+this.getUTCDate().toPaddedString(2)+"T"+this.getUTCHours().toPaddedString(2)+":"+this.getUTCMinutes().toPaddedString(2)+":"+this.getUTCSeconds().toPaddedString(2)+'Z"'};var Try={these:function(){var d;for(var c=0,f=arguments.length;c<f;c++){var a=arguments[c];try{d=a();break}catch(g){}}return d}};RegExp.prototype.match=RegExp.prototype.test;RegExp.escape=function(a){return String(a).replace(/([.*+?^=!:${}()|[\]\/\\])/g,"\\$1")};var PeriodicalExecuter=Class.create({initialize:function(c,a){this.callback=c;this.frequency=a;this.currentlyExecuting=false;this.registerCallback()},registerCallback:function(){this.timer=setInterval(this.onTimerEvent.bind(this),this.frequency*1000)},execute:function(){this.callback(this)},stop:function(){if(!this.timer){return}clearInterval(this.timer);this.timer=null},onTimerEvent:function(){if(!this.currentlyExecuting){try{this.currentlyExecuting=true;this.execute()}finally{this.currentlyExecuting=false}}}});Object.extend(String,{interpret:function(a){return a==null?"":String(a)},specialChar:{"\b":"\\b","\t":"\\t","\n":"\\n","\f":"\\f","\r":"\\r","\\":"\\\\"}});Object.extend(String.prototype,{gsub:function(f,d){var a="",e=this,c;d=arguments.callee.prepareReplacement(d);while(e.length>0){if(c=e.match(f)){a+=e.slice(0,c.index);a+=String.interpret(d(c));e=e.slice(c.index+c[0].length)}else{a+=e,e=""}}return a},sub:function(d,a,c){a=this.gsub.prepareReplacement(a);c=Object.isUndefined(c)?1:c;return this.gsub(d,function(e){if(--c<0){return e[0]}return a(e)})},scan:function(c,a){this.gsub(c,a);return String(this)},truncate:function(c,a){c=c||30;a=Object.isUndefined(a)?"...":a;return this.length>c?this.slice(0,c-a.length)+a:String(this)},strip:function(){return this.replace(/^\s+/,"").replace(/\s+$/,"")},stripTags:function(){return this.replace(/<\/?[^>]+>/gi,"")},stripScripts:function(){return this.replace(new RegExp(Prototype.ScriptFragment,"img"),"")},extractScripts:function(){var c=new RegExp(Prototype.ScriptFragment,"img");var a=new RegExp(Prototype.ScriptFragment,"im");return(this.match(c)||[]).map(function(d){return(d.match(a)||["",""])[1]})},evalScripts:function(){return this.extractScripts().map(function(script){return eval(script)})},escapeHTML:function(){var a=arguments.callee;a.text.data=this;return a.div.innerHTML},unescapeHTML:function(){var a=new Element("div");a.innerHTML=this.stripTags();return a.childNodes[0]?(a.childNodes.length>1?$A(a.childNodes).inject("",function(c,d){return c+d.nodeValue}):a.childNodes[0].nodeValue):""},toQueryParams:function(c){var a=this.strip().match(/([^?#]*)(#.*)?$/);if(!a){return{}}return a[1].split(c||"&").inject({},function(f,g){if((g=g.split("="))[0]){var d=decodeURIComponent(g.shift());var e=g.length>1?g.join("="):g[0];if(e!=undefined){e=decodeURIComponent(e)}if(d in f){if(!Object.isArray(f[d])){f[d]=[f[d]]}f[d].push(e)}else{f[d]=e}}return f})},toArray:function(){return this.split("")},succ:function(){return this.slice(0,this.length-1)+String.fromCharCode(this.charCodeAt(this.length-1)+1)},times:function(a){return a<1?"":new Array(a+1).join(this)},camelize:function(){var e=this.split("-"),a=e.length;if(a==1){return e[0]}var d=this.charAt(0)=="-"?e[0].charAt(0).toUpperCase()+e[0].substring(1):e[0];for(var c=1;c<a;c++){d+=e[c].charAt(0).toUpperCase()+e[c].substring(1)}return d},capitalize:function(){return this.charAt(0).toUpperCase()+this.substring(1).toLowerCase()},underscore:function(){return this.gsub(/::/,"/").gsub(/([A-Z]+)([A-Z][a-z])/,"#{1}_#{2}").gsub(/([a-z\d])([A-Z])/,"#{1}_#{2}").gsub(/-/,"_").toLowerCase()},dasherize:function(){return this.gsub(/_/,"-")},inspect:function(c){var a=this.gsub(/[\x00-\x1f\\]/,function(d){var e=String.specialChar[d[0]];return e?e:"\\u00"+d[0].charCodeAt().toPaddedString(2,16)});if(c){return'"'+a.replace(/"/g,'\\"')+'"'}return"'"+a.replace(/'/g,"\\'")+"'"},toJSON:function(){return this.inspect(true)},unfilterJSON:function(a){return this.sub(a||Prototype.JSONFilter,"#{1}")},isJSON:function(){var a=this;if(a.blank()){return false}a=this.replace(/\\./g,"@").replace(/"[^"\\\n\r]*"/g,"");return(/^[,:{}\[\]0-9.\-+Eaeflnr-u \n\r\t]*$/).test(a)},evalJSON:function(sanitize){var json=this.unfilterJSON();try{if(!sanitize||json.isJSON()){return eval("("+json+")")}}catch(e){}throw new SyntaxError("Badly formed JSON string: "+this.inspect())},include:function(a){return this.indexOf(a)>-1},startsWith:function(a){return this.indexOf(a)===0},endsWith:function(a){var c=this.length-a.length;return c>=0&&this.lastIndexOf(a)===c},empty:function(){return this==""},blank:function(){return/^\s*$/.test(this)},interpolate:function(a,c){return new Template(this,c).evaluate(a)}});if(Prototype.Browser.WebKit||Prototype.Browser.IE){Object.extend(String.prototype,{escapeHTML:function(){return this.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;")},unescapeHTML:function(){return this.stripTags().replace(/&amp;/g,"&").replace(/&lt;/g,"<").replace(/&gt;/g,">")}})}String.prototype.gsub.prepareReplacement=function(c){if(Object.isFunction(c)){return c}var a=new Template(c);return function(d){return a.evaluate(d)}};String.prototype.parseQuery=String.prototype.toQueryParams;Object.extend(String.prototype.escapeHTML,{div:document.createElement("div"),text:document.createTextNode("")});String.prototype.escapeHTML.div.appendChild(String.prototype.escapeHTML.text);var Template=Class.create({initialize:function(a,c){this.template=a.toString();this.pattern=c||Template.Pattern},evaluate:function(a){if(Object.isFunction(a.toTemplateReplacements)){a=a.toTemplateReplacements()}return this.template.gsub(this.pattern,function(e){if(a==null){return""}var g=e[1]||"";if(g=="\\"){return e[2]}var c=a,h=e[3];var f=/^([^.[]+|\[((?:.*?[^\\])?)\])(\.|\[|$)/;e=f.exec(h);if(e==null){return g}while(e!=null){var d=e[1].startsWith("[")?e[2].gsub("\\\\]","]"):e[1];c=c[d];if(null==c||""==e[3]){break}h=h.substring("["==e[3]?e[1].length:e[0].length);e=f.exec(h)}return g+String.interpret(c)})}});Template.Pattern=/(^|.|\r|\n)(#\{(.*?)\})/;var $break={};var Enumerable={each:function(d,c){var a=0;try{this._each(function(e){d.call(c,e,a++)})}catch(f){if(f!=$break){throw f}}return this},eachSlice:function(e,d,c){var a=-e,f=[],g=this.toArray();if(e<1){return g}while((a+=e)<g.length){f.push(g.slice(a,a+e))}return f.collect(d,c)},all:function(d,c){d=d||Prototype.K;var a=true;this.each(function(f,e){a=a&&!!d.call(c,f,e);if(!a){throw $break}});return a},any:function(d,c){d=d||Prototype.K;var a=false;this.each(function(f,e){if(a=!!d.call(c,f,e)){throw $break}});return a},collect:function(d,c){d=d||Prototype.K;var a=[];this.each(function(f,e){a.push(d.call(c,f,e))});return a},detect:function(d,c){var a;this.each(function(f,e){if(d.call(c,f,e)){a=f;throw $break}});return a},findAll:function(d,c){var a=[];this.each(function(f,e){if(d.call(c,f,e)){a.push(f)}});return a},grep:function(e,d,c){d=d||Prototype.K;var a=[];if(Object.isString(e)){e=new RegExp(e)}this.each(function(g,f){if(e.match(g)){a.push(d.call(c,g,f))}});return a},include:function(a){if(Object.isFunction(this.indexOf)){if(this.indexOf(a)!=-1){return true}}var c=false;this.each(function(d){if(d==a){c=true;throw $break}});return c},inGroupsOf:function(c,a){a=Object.isUndefined(a)?null:a;return this.eachSlice(c,function(d){while(d.length<c){d.push(a)}return d})},inject:function(a,d,c){this.each(function(f,e){a=d.call(c,a,f,e)});return a},invoke:function(c){var a=$A(arguments).slice(1);return this.map(function(d){return d[c].apply(d,a)})},max:function(d,c){d=d||Prototype.K;var a;this.each(function(f,e){f=d.call(c,f,e);if(a==null||f>=a){a=f}});return a},min:function(d,c){d=d||Prototype.K;var a;this.each(function(f,e){f=d.call(c,f,e);if(a==null||f<a){a=f}});return a},partition:function(e,c){e=e||Prototype.K;var d=[],a=[];this.each(function(g,f){(e.call(c,g,f)?d:a).push(g)});return[d,a]},pluck:function(c){var a=[];this.each(function(d){a.push(d[c])});return a},reject:function(d,c){var a=[];this.each(function(f,e){if(!d.call(c,f,e)){a.push(f)}});return a},sortBy:function(c,a){return this.map(function(e,d){return{value:e,criteria:c.call(a,e,d)}}).sort(function(g,f){var e=g.criteria,d=f.criteria;return e<d?-1:e>d?1:0}).pluck("value")},toArray:function(){return this.map()},zip:function(){var c=Prototype.K,a=$A(arguments);if(Object.isFunction(a.last())){c=a.pop()}var d=[this].concat(a).map($A);return this.map(function(f,e){return c(d.pluck(e))})},size:function(){return this.toArray().length},inspect:function(){return"#<Enumerable:"+this.toArray().inspect()+">"}};Object.extend(Enumerable,{map:Enumerable.collect,find:Enumerable.detect,select:Enumerable.findAll,filter:Enumerable.findAll,member:Enumerable.include,entries:Enumerable.toArray,every:Enumerable.all,some:Enumerable.any});function $A(d){if(!d){return[]}if(d.toArray){return d.toArray()}var c=d.length||0,a=new Array(c);while(c--){a[c]=d[c]}return a}if(Prototype.Browser.WebKit){$A=function(d){if(!d){return[]}if(!(typeof d==="function"&&typeof d.length==="number"&&typeof d.item==="function")&&d.toArray){return d.toArray()}var c=d.length||0,a=new Array(c);while(c--){a[c]=d[c]}return a}}Array.from=$A;Object.extend(Array.prototype,Enumerable);if(!Array.prototype._reverse){Array.prototype._reverse=Array.prototype.reverse}Object.extend(Array.prototype,{_each:function(c){for(var a=0,d=this.length;a<d;a++){c(this[a])}},clear:function(){this.length=0;return this},first:function(){return this[0]},last:function(){return this[this.length-1]},compact:function(){return this.select(function(a){return a!=null})},flatten:function(){return this.inject([],function(c,a){return c.concat(Object.isArray(a)?a.flatten():[a])})},without:function(){var a=$A(arguments);return this.select(function(c){return !a.include(c)})},reverse:function(a){return(a!==false?this:this.toArray())._reverse()},reduce:function(){return this.length>1?this:this[0]},uniq:function(a){return this.inject([],function(e,d,c){if(0==c||(a?e.last()!=d:!e.include(d))){e.push(d)}return e})},intersect:function(a){return this.uniq().findAll(function(c){return a.detect(function(d){return c===d})})},clone:function(){return[].concat(this)},size:function(){return this.length},inspect:function(){return"["+this.map(Object.inspect).join(", ")+"]"},toJSON:function(){var a=[];this.each(function(c){var d=Object.toJSON(c);if(!Object.isUndefined(d)){a.push(d)}});return"["+a.join(", ")+"]"}});if(Object.isFunction(Array.prototype.forEach)){Array.prototype._each=Array.prototype.forEach}if(!Array.prototype.indexOf){Array.prototype.indexOf=function(d,a){a||(a=0);var c=this.length;if(a<0){a=c+a}for(;a<c;a++){if(this[a]===d){return a}}return -1}}if(!Array.prototype.lastIndexOf){Array.prototype.lastIndexOf=function(c,a){a=isNaN(a)?this.length:(a<0?this.length+a:a)+1;var d=this.slice(0,a).reverse().indexOf(c);return(d<0)?d:a-d-1}}Array.prototype.toArray=Array.prototype.clone;function $w(a){if(!Object.isString(a)){return[]}a=a.strip();return a?a.split(/\s+/):[]}if(Prototype.Browser.Opera){Array.prototype.concat=function(){var f=[];for(var c=0,d=this.length;c<d;c++){f.push(this[c])}for(var c=0,d=arguments.length;c<d;c++){if(Object.isArray(arguments[c])){for(var a=0,e=arguments[c].length;a<e;a++){f.push(arguments[c][a])}}else{f.push(arguments[c])}}return f}}Object.extend(Number.prototype,{toColorPart:function(){return this.toPaddedString(2,16)},succ:function(){return this+1},times:function(c,a){$R(0,this,true).each(c,a);return this},toPaddedString:function(d,c){var a=this.toString(c||10);return"0".times(d-a.length)+a},toJSON:function(){return isFinite(this)?this.toString():"null"}});$w("abs round ceil floor").each(function(a){Number.prototype[a]=Math[a].methodize()});function $H(a){return new Hash(a)}var Hash=Class.create(Enumerable,(function(){function a(c,d){if(Object.isUndefined(d)){return c}return c+"="+encodeURIComponent(String.interpret(d))}return{initialize:function(c){this._object=Object.isHash(c)?c.toObject():Object.clone(c)},_each:function(d){for(var c in this._object){var e=this._object[c],f=[c,e];f.key=c;f.value=e;d(f)}},set:function(c,d){return this._object[c]=d},get:function(c){if(this._object[c]!==Object.prototype[c]){return this._object[c]}},unset:function(c){var d=this._object[c];delete this._object[c];return d},toObject:function(){return Object.clone(this._object)},keys:function(){return this.pluck("key")},values:function(){return this.pluck("value")},index:function(d){var c=this.detect(function(e){return e.value===d});return c&&c.key},merge:function(c){return this.clone().update(c)},update:function(c){return new Hash(c).inject(this,function(d,e){d.set(e.key,e.value);return d})},toQueryString:function(){return this.inject([],function(e,f){var d=encodeURIComponent(f.key),c=f.value;if(c&&typeof c=="object"){if(Object.isArray(c)){return e.concat(c.map(a.curry(d)))}}else{e.push(a(d,c))}return e}).join("&")},inspect:function(){return"#<Hash:{"+this.map(function(c){return c.map(Object.inspect).join(": ")}).join(", ")+"}>"},toJSON:function(){return Object.toJSON(this.toObject())},clone:function(){return new Hash(this)}}})());Hash.prototype.toTemplateReplacements=Hash.prototype.toObject;Hash.from=$H;var ObjectRange=Class.create(Enumerable,{initialize:function(d,a,c){this.start=d;this.end=a;this.exclusive=c},_each:function(a){var c=this.start;while(this.include(c)){a(c);c=c.succ()}},include:function(a){if(a<this.start){return false}if(this.exclusive){return a<this.end}return a<=this.end}});var $R=function(d,a,c){return new ObjectRange(d,a,c)};var Ajax={getTransport:function(){return Try.these(function(){return new XMLHttpRequest()},function(){return new ActiveXObject("Msxml2.XMLHTTP")},function(){return new ActiveXObject("Microsoft.XMLHTTP")})||false},activeRequestCount:0};Ajax.Responders={responders:[],_each:function(a){this.responders._each(a)},register:function(a){if(!this.include(a)){this.responders.push(a)}},unregister:function(a){this.responders=this.responders.without(a)},dispatch:function(e,c,d,a){this.each(function(f){if(Object.isFunction(f[e])){try{f[e].apply(f,[c,d,a])}catch(g){}}})}};Object.extend(Ajax.Responders,Enumerable);Ajax.Responders.register({onCreate:function(){Ajax.activeRequestCount++},onComplete:function(){Ajax.activeRequestCount--}});Ajax.Base=Class.create({initialize:function(a){this.options={method:"post",asynchronous:true,contentType:"application/x-www-form-urlencoded",encoding:"UTF-8",parameters:"",evalJSON:true,evalJS:true};Object.extend(this.options,a||{});this.options.method=this.options.method.toLowerCase();if(Object.isString(this.options.parameters)){this.options.parameters=this.options.parameters.toQueryParams()}else{if(Object.isHash(this.options.parameters)){this.options.parameters=this.options.parameters.toObject()}}}});Ajax.Request=Class.create(Ajax.Base,{_complete:false,initialize:function($super,c,a){$super(a);this.transport=Ajax.getTransport();this.request(c)},request:function(c){this.url=c;this.method=this.options.method;var f=Object.clone(this.options.parameters);if(!["get","post"].include(this.method)){f._method=this.method;this.method="post"}this.parameters=f;if(f=Object.toQueryString(f)){if(this.method=="get"){this.url+=(this.url.include("?")?"&":"?")+f}else{if(/Konqueror|Safari|KHTML/.test(navigator.userAgent)){f+="&_="}}}try{var a=new Ajax.Response(this);if(this.options.onCreate){this.options.onCreate(a)}Ajax.Responders.dispatch("onCreate",this,a);this.transport.open(this.method.toUpperCase(),this.url,this.options.asynchronous);if(this.options.asynchronous){this.respondToReadyState.bind(this).defer(1)}this.transport.onreadystatechange=this.onStateChange.bind(this);this.setRequestHeaders();this.body=this.method=="post"?(this.options.postBody||f):null;this.transport.send(this.body);if(!this.options.asynchronous&&this.transport.overrideMimeType){this.onStateChange()}}catch(d){this.dispatchException(d)}},onStateChange:function(){var a=this.transport.readyState;if(a>1&&!((a==4)&&this._complete)){this.respondToReadyState(this.transport.readyState)}},setRequestHeaders:function(){var f={"X-Requested-With":"XMLHttpRequest","X-Prototype-Version":Prototype.Version,Accept:"text/javascript, text/html, application/xml, text/xml, */*"};if(this.method=="post"){f["Content-type"]=this.options.contentType+(this.options.encoding?"; charset="+this.options.encoding:"");if(this.transport.overrideMimeType&&(navigator.userAgent.match(/Gecko\/(\d{4})/)||[0,2005])[1]<2005){f.Connection="close"}}if(typeof this.options.requestHeaders=="object"){var d=this.options.requestHeaders;if(Object.isFunction(d.push)){for(var c=0,e=d.length;c<e;c+=2){f[d[c]]=d[c+1]}}else{$H(d).each(function(g){f[g.key]=g.value})}}for(var a in f){this.transport.setRequestHeader(a,f[a])}},success:function(){var a=this.getStatus();return !a||(a>=200&&a<300)},getStatus:function(){try{return this.transport.status||0}catch(a){return 0}},respondToReadyState:function(a){var d=Ajax.Request.Events[a],c=new Ajax.Response(this);if(d=="Complete"){try{this._complete=true;(this.options["on"+c.status]||this.options["on"+(this.success()?"Success":"Failure")]||Prototype.emptyFunction)(c,c.headerJSON)}catch(f){this.dispatchException(f)}var g=c.getHeader("Content-type");if(this.options.evalJS=="force"||(this.options.evalJS&&this.isSameOrigin()&&g&&g.match(/^\s*(text|application)\/(x-)?(java|ecma)script(;.*)?\s*$/i))){this.evalResponse()}}try{(this.options["on"+d]||Prototype.emptyFunction)(c,c.headerJSON);Ajax.Responders.dispatch("on"+d,this,c,c.headerJSON)}catch(f){this.dispatchException(f)}if(d=="Complete"){this.transport.onreadystatechange=Prototype.emptyFunction}},isSameOrigin:function(){var a=this.url.match(/^\s*https?:\/\/[^\/]*/);return !a||(a[0]=="#{protocol}//#{domain}#{port}".interpolate({protocol:location.protocol,domain:document.domain,port:location.port?":"+location.port:""}))},getHeader:function(a){try{return this.transport.getResponseHeader(a)||null}catch(c){return null}},evalResponse:function(){try{return eval((this.transport.responseText||"").unfilterJSON())}catch(e){this.dispatchException(e)}},dispatchException:function(a){(this.options.onException||Prototype.emptyFunction)(this,a);Ajax.Responders.dispatch("onException",this,a)}});Ajax.Request.Events=["Uninitialized","Loading","Loaded","Interactive","Complete"];Ajax.Response=Class.create({initialize:function(d){this.request=d;var e=this.transport=d.transport,a=this.readyState=e.readyState;if((a>2&&!Prototype.Browser.IE)||a==4){this.status=this.getStatus();this.statusText=this.getStatusText();this.responseText=String.interpret(e.responseText);this.headerJSON=this._getHeaderJSON()}if(a==4){var c=e.responseXML;this.responseXML=Object.isUndefined(c)?null:c;this.responseJSON=this._getResponseJSON()}},status:0,statusText:"",getStatus:Ajax.Request.prototype.getStatus,getStatusText:function(){try{return this.transport.statusText||""}catch(a){return""}},getHeader:Ajax.Request.prototype.getHeader,getAllHeaders:function(){try{return this.getAllResponseHeaders()}catch(a){return null}},getResponseHeader:function(a){return this.transport.getResponseHeader(a)},getAllResponseHeaders:function(){return this.transport.getAllResponseHeaders()},_getHeaderJSON:function(){var a=this.getHeader("X-JSON");if(!a){return null}a=decodeURIComponent(escape(a));try{return a.evalJSON(this.request.options.sanitizeJSON||!this.request.isSameOrigin())}catch(c){this.request.dispatchException(c)}},_getResponseJSON:function(){var a=this.request.options;if(!a.evalJSON||(a.evalJSON!="force"&&!(this.getHeader("Content-type")||"").include("application/json"))||this.responseText.blank()){return null}try{return this.responseText.evalJSON(a.sanitizeJSON||!this.request.isSameOrigin())}catch(c){this.request.dispatchException(c)}}});Ajax.Updater=Class.create(Ajax.Request,{initialize:function($super,a,d,c){this.container={success:(a.success||a),failure:(a.failure||(a.success?null:a))};c=Object.clone(c);var e=c.onComplete;c.onComplete=(function(f,g){this.updateContent(f.responseText);if(Object.isFunction(e)){e(f,g)}}).bind(this);$super(d,c)},updateContent:function(e){var d=this.container[this.success()?"success":"failure"],a=this.options;if(!a.evalScripts){e=e.stripScripts()}if(d=$(d)){if(a.insertion){if(Object.isString(a.insertion)){var c={};c[a.insertion]=e;d.insert(c)}else{a.insertion(d,e)}}else{d.update(e)}}}});Ajax.PeriodicalUpdater=Class.create(Ajax.Base,{initialize:function($super,a,d,c){$super(c);this.onComplete=this.options.onComplete;this.frequency=(this.options.frequency||2);this.decay=(this.options.decay||1);this.updater={};this.container=a;this.url=d;this.start()},start:function(){this.options.onComplete=this.updateComplete.bind(this);this.onTimerEvent()},stop:function(){this.updater.options.onComplete=undefined;clearTimeout(this.timer);(this.onComplete||Prototype.emptyFunction).apply(this,arguments)},updateComplete:function(a){if(this.options.decay){this.decay=(a.responseText==this.lastText?this.decay*this.options.decay:1);this.lastText=a.responseText}this.timer=this.onTimerEvent.bind(this).delay(this.decay*this.frequency)},onTimerEvent:function(){this.updater=new Ajax.Updater(this.container,this.url,this.options)}});function $(c){if(arguments.length>1){for(var a=0,e=[],d=arguments.length;a<d;a++){e.push($(arguments[a]))}return e}if(Object.isString(c)){c=document.getElementById(c)}return Element.extend(c)}if(Prototype.BrowserFeatures.XPath){document._getElementsByXPath=function(g,a){var d=[];var f=document.evaluate(g,$(a)||document,null,XPathResult.ORDERED_NODE_SNAPSHOT_TYPE,null);for(var c=0,e=f.snapshotLength;c<e;c++){d.push(Element.extend(f.snapshotItem(c)))}return d}}if(!window.Node){var Node={}}if(!Node.ELEMENT_NODE){Object.extend(Node,{ELEMENT_NODE:1,ATTRIBUTE_NODE:2,TEXT_NODE:3,CDATA_SECTION_NODE:4,ENTITY_REFERENCE_NODE:5,ENTITY_NODE:6,PROCESSING_INSTRUCTION_NODE:7,COMMENT_NODE:8,DOCUMENT_NODE:9,DOCUMENT_TYPE_NODE:10,DOCUMENT_FRAGMENT_NODE:11,NOTATION_NODE:12})}(function(){var a=this.Element;this.Element=function(e,d){d=d||{};e=e.toLowerCase();var c=Element.cache;if(Prototype.Browser.IE&&d.name){e="<"+e+' name="'+d.name+'">';delete d.name;return Element.writeAttribute(document.createElement(e),d)}if(!c[e]){c[e]=Element.extend(document.createElement(e))}return Element.writeAttribute(c[e].cloneNode(false),d)};Object.extend(this.Element,a||{});if(a){this.Element.prototype=a.prototype}}).call(window);Element.cache={};Element.Methods={visible:function(a){return $(a).style.display!="none"},toggle:function(a){a=$(a);Element[Element.visible(a)?"hide":"show"](a);return a},hide:function(a){a=$(a);a.style.display="none";return a},show:function(a){a=$(a);a.style.display="";return a},remove:function(a){a=$(a);a.parentNode.removeChild(a);return a},update:function(a,c){a=$(a);if(c&&c.toElement){c=c.toElement()}if(Object.isElement(c)){return a.update().insert(c)}c=Object.toHTML(c);a.innerHTML=c.stripScripts();c.evalScripts.bind(c).defer();return a},replace:function(c,d){c=$(c);if(d&&d.toElement){d=d.toElement()}else{if(!Object.isElement(d)){d=Object.toHTML(d);var a=c.ownerDocument.createRange();a.selectNode(c);d.evalScripts.bind(d).defer();d=a.createContextualFragment(d.stripScripts())}}c.parentNode.replaceChild(d,c);return c},insert:function(d,f){d=$(d);if(Object.isString(f)||Object.isNumber(f)||Object.isElement(f)||(f&&(f.toElement||f.toHTML))){f={bottom:f}}var e,g,c,h;for(var a in f){e=f[a];a=a.toLowerCase();g=Element._insertionTranslations[a];if(e&&e.toElement){e=e.toElement()}if(Object.isElement(e)){g(d,e);continue}e=Object.toHTML(e);c=((a=="before"||a=="after")?d.parentNode:d).tagName.toUpperCase();h=Element._getContentFromAnonymousElement(c,e.stripScripts());if(a=="top"||a=="after"){h.reverse()}h.each(g.curry(d));e.evalScripts.bind(e).defer()}return d},wrap:function(c,d,a){c=$(c);if(Object.isElement(d)){$(d).writeAttribute(a||{})}else{if(Object.isString(d)){d=new Element(d,a)}else{d=new Element("div",d)}}if(c.parentNode){c.parentNode.replaceChild(d,c)}d.appendChild(c);return d},inspect:function(c){c=$(c);var a="<"+c.tagName.toLowerCase();$H({id:"id",className:"class"}).each(function(g){var f=g.first(),d=g.last();var e=(c[f]||"").toString();if(e){a+=" "+d+"="+e.inspect(true)}});return a+">"},recursivelyCollect:function(a,d){a=$(a);var c=[];while(a=a[d]){if(a.nodeType==1){c.push(Element.extend(a))}}return c},ancestors:function(a){return $(a).recursivelyCollect("parentNode")},descendants:function(a){return $(a).select("*")},firstDescendant:function(a){a=$(a).firstChild;while(a&&a.nodeType!=1){a=a.nextSibling}return $(a)},immediateDescendants:function(a){if(!(a=$(a).firstChild)){return[]}while(a&&a.nodeType!=1){a=a.nextSibling}if(a){return[a].concat($(a).nextSiblings())}return[]},previousSiblings:function(a){return $(a).recursivelyCollect("previousSibling")},nextSiblings:function(a){return $(a).recursivelyCollect("nextSibling")},siblings:function(a){a=$(a);return a.previousSiblings().reverse().concat(a.nextSiblings())},match:function(c,a){if(Object.isString(a)){a=new Selector(a)}return a.match($(c))},up:function(c,e,a){c=$(c);if(arguments.length==1){return $(c.parentNode)}var d=c.ancestors();return Object.isNumber(e)?d[e]:Selector.findElement(d,e,a)},down:function(c,d,a){c=$(c);if(arguments.length==1){return c.firstDescendant()}return Object.isNumber(d)?c.descendants()[d]:Element.select(c,d)[a||0]},previous:function(c,e,a){c=$(c);if(arguments.length==1){return $(Selector.handlers.previousElementSibling(c))}var d=c.previousSiblings();return Object.isNumber(e)?d[e]:Selector.findElement(d,e,a)},next:function(d,e,c){d=$(d);if(arguments.length==1){return $(Selector.handlers.nextElementSibling(d))}var a=d.nextSiblings();return Object.isNumber(e)?a[e]:Selector.findElement(a,e,c)},select:function(){var a=$A(arguments),c=$(a.shift());return Selector.findChildElements(c,a)},adjacent:function(){var a=$A(arguments),c=$(a.shift());return Selector.findChildElements(c.parentNode,a).without(c)},identify:function(c){c=$(c);var d=c.readAttribute("id"),a=arguments.callee;if(d){return d}do{d="anonymous_element_"+a.counter++}while($(d));c.writeAttribute("id",d);return d},readAttribute:function(d,a){d=$(d);if(Prototype.Browser.IE){var c=Element._attributeTranslations.read;if(c.values[a]){return c.values[a](d,a)}if(c.names[a]){a=c.names[a]}if(a.include(":")){return(!d.attributes||!d.attributes[a])?null:d.attributes[a].value}}return d.getAttribute(a)},writeAttribute:function(f,d,g){f=$(f);var c={},e=Element._attributeTranslations.write;if(typeof d=="object"){c=d}else{c[d]=Object.isUndefined(g)?true:g}for(var a in c){d=e.names[a]||a;g=c[a];if(e.values[a]){d=e.values[a](f,g)}if(g===false||g===null){f.removeAttribute(d)}else{if(g===true){f.setAttribute(d,d)}else{f.setAttribute(d,g)}}}return f},getHeight:function(a){return $(a).getDimensions().height},getWidth:function(a){return $(a).getDimensions().width},classNames:function(a){return new Element.ClassNames(a)},hasClassName:function(a,c){if(!(a=$(a))){return}var d=a.className;return(d.length>0&&(d==c||new RegExp("(^|\\s)"+c+"(\\s|$)").test(d)))},addClassName:function(a,c){if(!(a=$(a))){return}if(!a.hasClassName(c)){a.className+=(a.className?" ":"")+c}return a},removeClassName:function(a,c){if(!(a=$(a))){return}a.className=a.className.replace(new RegExp("(^|\\s+)"+c+"(\\s+|$)")," ").strip();return a},toggleClassName:function(a,c){if(!(a=$(a))){return}return a[a.hasClassName(c)?"removeClassName":"addClassName"](c)},cleanWhitespace:function(c){c=$(c);var d=c.firstChild;while(d){var a=d.nextSibling;if(d.nodeType==3&&!/\S/.test(d.nodeValue)){c.removeChild(d)}d=a}return c},empty:function(a){return $(a).innerHTML.blank()},descendantOf:function(c,a){c=$(c),a=$(a);if(c.compareDocumentPosition){return(c.compareDocumentPosition(a)&8)===8}if(a.contains){return a.contains(c)&&a!==c}while(c=c.parentNode){if(c==a){return true}}return false},scrollTo:function(a){a=$(a);var c=a.cumulativeOffset();window.scrollTo(c[0],c[1]);return a},getStyle:function(c,d){c=$(c);d=d=="float"?"cssFloat":d.camelize();var e=c.style[d];if(!e||e=="auto"){var a=document.defaultView.getComputedStyle(c,null);e=a?a[d]:null}if(d=="opacity"){return e?parseFloat(e):1}return e=="auto"?null:e},getOpacity:function(a){return $(a).getStyle("opacity")},setStyle:function(c,d){c=$(c);var f=c.style,a;if(Object.isString(d)){c.style.cssText+=";"+d;return d.include("opacity")?c.setOpacity(d.match(/opacity:\s*(\d?\.?\d*)/)[1]):c}for(var e in d){if(e=="opacity"){c.setOpacity(d[e])}else{f[(e=="float"||e=="cssFloat")?(Object.isUndefined(f.styleFloat)?"cssFloat":"styleFloat"):e]=d[e]}}return c},setOpacity:function(a,c){a=$(a);a.style.opacity=(c==1||c==="")?"":(c<0.00001)?0:c;return a},getDimensions:function(d){d=$(d);var h=d.getStyle("display");if(h!="none"&&h!=null){return{width:d.offsetWidth,height:d.offsetHeight}}var c=d.style;var g=c.visibility;var e=c.position;var a=c.display;c.visibility="hidden";c.position="absolute";c.display="block";var j=d.clientWidth;var f=d.clientHeight;c.display=a;c.position=e;c.visibility=g;return{width:j,height:f}},makePositioned:function(a){a=$(a);var c=Element.getStyle(a,"position");if(c=="static"||!c){a._madePositioned=true;a.style.position="relative";if(Prototype.Browser.Opera){a.style.top=0;a.style.left=0}}return a},undoPositioned:function(a){a=$(a);if(a._madePositioned){a._madePositioned=undefined;a.style.position=a.style.top=a.style.left=a.style.bottom=a.style.right=""}return a},makeClipping:function(a){a=$(a);if(a._overflow){return a}a._overflow=Element.getStyle(a,"overflow")||"auto";if(a._overflow!=="hidden"){a.style.overflow="hidden"}return a},undoClipping:function(a){a=$(a);if(!a._overflow){return a}a.style.overflow=a._overflow=="auto"?"":a._overflow;a._overflow=null;return a},cumulativeOffset:function(c){var a=0,d=0;do{a+=c.offsetTop||0;d+=c.offsetLeft||0;c=c.offsetParent}while(c);return Element._returnOffset(d,a)},positionedOffset:function(c){var a=0,e=0;do{a+=c.offsetTop||0;e+=c.offsetLeft||0;c=c.offsetParent;if(c){if(c.tagName.toUpperCase()=="BODY"){break}var d=Element.getStyle(c,"position");if(d!=="static"){break}}}while(c);return Element._returnOffset(e,a)},absolutize:function(c){c=$(c);if(c.getStyle("position")=="absolute"){return c}var e=c.positionedOffset();var g=e[1];var f=e[0];var d=c.clientWidth;var a=c.clientHeight;c._originalLeft=f-parseFloat(c.style.left||0);c._originalTop=g-parseFloat(c.style.top||0);c._originalWidth=c.style.width;c._originalHeight=c.style.height;c.style.position="absolute";c.style.top=g+"px";c.style.left=f+"px";c.style.width=d+"px";c.style.height=a+"px";return c},relativize:function(a){a=$(a);if(a.getStyle("position")=="relative"){return a}a.style.position="relative";var d=parseFloat(a.style.top||0)-(a._originalTop||0);var c=parseFloat(a.style.left||0)-(a._originalLeft||0);a.style.top=d+"px";a.style.left=c+"px";a.style.height=a._originalHeight;a.style.width=a._originalWidth;return a},cumulativeScrollOffset:function(c){var a=0,d=0;do{a+=c.scrollTop||0;d+=c.scrollLeft||0;c=c.parentNode}while(c);return Element._returnOffset(d,a)},getOffsetParent:function(a){if(a.offsetParent){return $(a.offsetParent)}if(a==document.body){return $(a)}while((a=a.parentNode)&&a!=document.body){if(Element.getStyle(a,"position")!="static"){return $(a)}}return $(document.body)},viewportOffset:function(e){var a=0,d=0;var c=e;do{a+=c.offsetTop||0;d+=c.offsetLeft||0;if(c.offsetParent==document.body&&Element.getStyle(c,"position")=="absolute"){break}}while(c=c.offsetParent);c=e;do{if(!Prototype.Browser.Opera||(c.tagName&&(c.tagName.toUpperCase()=="BODY"))){a-=c.scrollTop||0;d-=c.scrollLeft||0}}while(c=c.parentNode);return Element._returnOffset(d,a)},clonePosition:function(c,e){var a=Object.extend({setLeft:true,setTop:true,setWidth:true,setHeight:true,offsetTop:0,offsetLeft:0},arguments[2]||{});e=$(e);var f=e.viewportOffset();c=$(c);var g=[0,0];var d=null;if(Element.getStyle(c,"position")=="absolute"){d=c.getOffsetParent();g=d.viewportOffset()}if(d==document.body){g[0]-=document.body.offsetLeft;g[1]-=document.body.offsetTop}if(a.setLeft){c.style.left=(f[0]-g[0]+a.offsetLeft)+"px"}if(a.setTop){c.style.top=(f[1]-g[1]+a.offsetTop)+"px"}if(a.setWidth){c.style.width=e.offsetWidth+"px"}if(a.setHeight){c.style.height=e.offsetHeight+"px"}return c}};Element.Methods.identify.counter=1;Object.extend(Element.Methods,{getElementsBySelector:Element.Methods.select,childElements:Element.Methods.immediateDescendants});Element._attributeTranslations={write:{names:{className:"class",htmlFor:"for"},values:{}}};if(Prototype.Browser.Opera){Element.Methods.getStyle=Element.Methods.getStyle.wrap(function(e,c,d){switch(d){case"left":case"top":case"right":case"bottom":if(e(c,"position")==="static"){return null}case"height":case"width":if(!Element.visible(c)){return null}var f=parseInt(e(c,d),10);if(f!==c["offset"+d.capitalize()]){return f+"px"}var a;if(d==="height"){a=["border-top-width","padding-top","padding-bottom","border-bottom-width"]}else{a=["border-left-width","padding-left","padding-right","border-right-width"]}return a.inject(f,function(g,h){var j=e(c,h);return j===null?g:g-parseInt(j,10)})+"px";default:return e(c,d)}});Element.Methods.readAttribute=Element.Methods.readAttribute.wrap(function(d,a,c){if(c==="title"){return a.title}return d(a,c)})}else{if(Prototype.Browser.IE){Element.Methods.getOffsetParent=Element.Methods.getOffsetParent.wrap(function(d,c){c=$(c);try{c.offsetParent}catch(g){return $(document.body)}var a=c.getStyle("position");if(a!=="static"){return d(c)}c.setStyle({position:"relative"});var f=d(c);c.setStyle({position:a});return f});$w("positionedOffset viewportOffset").each(function(a){Element.Methods[a]=Element.Methods[a].wrap(function(g,d){d=$(d);try{d.offsetParent}catch(j){return Element._returnOffset(0,0)}var c=d.getStyle("position");if(c!=="static"){return g(d)}var f=d.getOffsetParent();if(f&&f.getStyle("position")==="fixed"){f.setStyle({zoom:1})}d.setStyle({position:"relative"});var h=g(d);d.setStyle({position:c});return h})});Element.Methods.cumulativeOffset=Element.Methods.cumulativeOffset.wrap(function(c,a){try{a.offsetParent}catch(d){return Element._returnOffset(0,0)}return c(a)});Element.Methods.getStyle=function(a,c){a=$(a);c=(c=="float"||c=="cssFloat")?"styleFloat":c.camelize();var d=a.style[c];if(!d&&a.currentStyle){d=a.currentStyle[c]}if(c=="opacity"){if(d=(a.getStyle("filter")||"").match(/alpha\(opacity=(.*)\)/)){if(d[1]){return parseFloat(d[1])/100}}return 1}if(d=="auto"){if((c=="width"||c=="height")&&(a.getStyle("display")!="none")){return a["offset"+c.capitalize()]+"px"}return null}return d};Element.Methods.setOpacity=function(c,f){function g(h){return h.replace(/alpha\([^\)]*\)/gi,"")}c=$(c);var a=c.currentStyle;if((a&&!a.hasLayout)||(!a&&c.style.zoom=="normal")){c.style.zoom=1}var e=c.getStyle("filter"),d=c.style;if(f==1||f===""){(e=g(e))?d.filter=e:d.removeAttribute("filter");return c}else{if(f<0.00001){f=0}}d.filter=g(e)+"alpha(opacity="+(f*100)+")";return c};Element._attributeTranslations={read:{names:{"class":"className","for":"htmlFor"},values:{_getAttr:function(a,c){return a.getAttribute(c,2)},_getAttrNode:function(a,d){var c=a.getAttributeNode(d);return c?c.value:""},_getEv:function(a,c){c=a.getAttribute(c);return c?c.toString().slice(23,-2):null},_flag:function(a,c){return $(a).hasAttribute(c)?c:null},style:function(a){return a.style.cssText.toLowerCase()},title:function(a){return a.title}}}};Element._attributeTranslations.write={names:Object.extend({cellpadding:"cellPadding",cellspacing:"cellSpacing"},Element._attributeTranslations.read.names),values:{checked:function(a,c){a.checked=!!c},style:function(a,c){a.style.cssText=c?c:""}}};Element._attributeTranslations.has={};$w("colSpan rowSpan vAlign dateTime accessKey tabIndex encType maxLength readOnly longDesc frameBorder").each(function(a){Element._attributeTranslations.write.names[a.toLowerCase()]=a;Element._attributeTranslations.has[a.toLowerCase()]=a});(function(a){Object.extend(a,{href:a._getAttr,src:a._getAttr,type:a._getAttr,action:a._getAttrNode,disabled:a._flag,checked:a._flag,readonly:a._flag,multiple:a._flag,onload:a._getEv,onunload:a._getEv,onclick:a._getEv,ondblclick:a._getEv,onmousedown:a._getEv,onmouseup:a._getEv,onmouseover:a._getEv,onmousemove:a._getEv,onmouseout:a._getEv,onfocus:a._getEv,onblur:a._getEv,onkeypress:a._getEv,onkeydown:a._getEv,onkeyup:a._getEv,onsubmit:a._getEv,onreset:a._getEv,onselect:a._getEv,onchange:a._getEv})})(Element._attributeTranslations.read.values)}else{if(Prototype.Browser.Gecko&&/rv:1\.8\.0/.test(navigator.userAgent)){Element.Methods.setOpacity=function(a,c){a=$(a);a.style.opacity=(c==1)?0.999999:(c==="")?"":(c<0.00001)?0:c;return a}}else{if(Prototype.Browser.WebKit){Element.Methods.setOpacity=function(a,c){a=$(a);a.style.opacity=(c==1||c==="")?"":(c<0.00001)?0:c;if(c==1){if(a.tagName.toUpperCase()=="IMG"&&a.width){a.width++;a.width--}else{try{var f=document.createTextNode(" ");a.appendChild(f);a.removeChild(f)}catch(d){}}}return a};Element.Methods.cumulativeOffset=function(c){var a=0,d=0;do{a+=c.offsetTop||0;d+=c.offsetLeft||0;if(c.offsetParent==document.body){if(Element.getStyle(c,"position")=="absolute"){break}}c=c.offsetParent}while(c);return Element._returnOffset(d,a)}}}}}if(Prototype.Browser.IE||Prototype.Browser.Opera){Element.Methods.update=function(c,d){c=$(c);if(d&&d.toElement){d=d.toElement()}if(Object.isElement(d)){return c.update().insert(d)}d=Object.toHTML(d);var a=c.tagName.toUpperCase();if(a in Element._insertionTranslations.tags){$A(c.childNodes).each(function(e){c.removeChild(e)});Element._getContentFromAnonymousElement(a,d.stripScripts()).each(function(e){c.appendChild(e)})}else{c.innerHTML=d.stripScripts()}d.evalScripts.bind(d).defer();return c}}if("outerHTML" in document.createElement("div")){Element.Methods.replace=function(d,f){d=$(d);if(f&&f.toElement){f=f.toElement()}if(Object.isElement(f)){d.parentNode.replaceChild(f,d);return d}f=Object.toHTML(f);var e=d.parentNode,c=e.tagName.toUpperCase();if(Element._insertionTranslations.tags[c]){var g=d.next();var a=Element._getContentFromAnonymousElement(c,f.stripScripts());e.removeChild(d);if(g){a.each(function(h){e.insertBefore(h,g)})}else{a.each(function(h){e.appendChild(h)})}}else{d.outerHTML=f.stripScripts()}f.evalScripts.bind(f).defer();return d}}Element._returnOffset=function(c,d){var a=[c,d];a.left=c;a.top=d;return a};Element._getContentFromAnonymousElement=function(d,c){var e=new Element("div"),a=Element._insertionTranslations.tags[d];if(a){e.innerHTML=a[0]+c+a[1];a[2].times(function(){e=e.firstChild})}else{e.innerHTML=c}return $A(e.childNodes)};Element._insertionTranslations={before:function(a,c){a.parentNode.insertBefore(c,a)},top:function(a,c){a.insertBefore(c,a.firstChild)},bottom:function(a,c){a.appendChild(c)},after:function(a,c){a.parentNode.insertBefore(c,a.nextSibling)},tags:{TABLE:["<table>","</table>",1],TBODY:["<table><tbody>","</tbody></table>",2],TR:["<table><tbody><tr>","</tr></tbody></table>",3],TD:["<table><tbody><tr><td>","</td></tr></tbody></table>",4],SELECT:["<select>","</select>",1]}};(function(){Object.extend(this.tags,{THEAD:this.tags.TBODY,TFOOT:this.tags.TBODY,TH:this.tags.TD})}).call(Element._insertionTranslations);Element.Methods.Simulated={hasAttribute:function(a,d){d=Element._attributeTranslations.has[d]||d;var c=$(a).getAttributeNode(d);return !!(c&&c.specified)}};Element.Methods.ByTag={};Object.extend(Element,Element.Methods);if(!Prototype.BrowserFeatures.ElementExtensions&&document.createElement("div")["__proto__"]){window.HTMLElement={};window.HTMLElement.prototype=document.createElement("div")["__proto__"];Prototype.BrowserFeatures.ElementExtensions=true}Element.extend=(function(){if(Prototype.BrowserFeatures.SpecificElementExtensions){return Prototype.K}var a={},c=Element.Methods.ByTag;var d=Object.extend(function(g){if(!g||g._extendedByPrototype||g.nodeType!=1||g==window){return g}var e=Object.clone(a),f=g.tagName.toUpperCase(),j,h;if(c[f]){Object.extend(e,c[f])}for(j in e){h=e[j];if(Object.isFunction(h)&&!(j in g)){g[j]=h.methodize()}}g._extendedByPrototype=Prototype.emptyFunction;return g},{refresh:function(){if(!Prototype.BrowserFeatures.ElementExtensions){Object.extend(a,Element.Methods);Object.extend(a,Element.Methods.Simulated)}}});d.refresh();return d})();Element.hasAttribute=function(a,c){if(a.hasAttribute){return a.hasAttribute(c)}return Element.Methods.Simulated.hasAttribute(a,c)};Element.addMethods=function(d){var j=Prototype.BrowserFeatures,e=Element.Methods.ByTag;if(!d){Object.extend(Form,Form.Methods);Object.extend(Form.Element,Form.Element.Methods);Object.extend(Element.Methods.ByTag,{FORM:Object.clone(Form.Methods),INPUT:Object.clone(Form.Element.Methods),SELECT:Object.clone(Form.Element.Methods),TEXTAREA:Object.clone(Form.Element.Methods)})}if(arguments.length==2){var c=d;d=arguments[1]}if(!c){Object.extend(Element.Methods,d||{})}else{if(Object.isArray(c)){c.each(h)}else{h(c)}}function h(l){l=l.toUpperCase();if(!Element.Methods.ByTag[l]){Element.Methods.ByTag[l]={}}Object.extend(Element.Methods.ByTag[l],d)}function a(o,m,l){l=l||false;for(var r in o){var q=o[r];if(!Object.isFunction(q)){continue}if(!l||!(r in m)){m[r]=q.methodize()}}}function f(o){var l;var m={OPTGROUP:"OptGroup",TEXTAREA:"TextArea",P:"Paragraph",FIELDSET:"FieldSet",UL:"UList",OL:"OList",DL:"DList",DIR:"Directory",H1:"Heading",H2:"Heading",H3:"Heading",H4:"Heading",H5:"Heading",H6:"Heading",Q:"Quote",INS:"Mod",DEL:"Mod",A:"Anchor",IMG:"Image",CAPTION:"TableCaption",COL:"TableCol",COLGROUP:"TableCol",THEAD:"TableSection",TFOOT:"TableSection",TBODY:"TableSection",TR:"TableRow",TH:"TableCell",TD:"TableCell",FRAMESET:"FrameSet",IFRAME:"IFrame"};if(m[o]){l="HTML"+m[o]+"Element"}if(window[l]){return window[l]}l="HTML"+o+"Element";if(window[l]){return window[l]}l="HTML"+o.capitalize()+"Element";if(window[l]){return window[l]}window[l]={};window[l].prototype=document.createElement(o)["__proto__"];return window[l]}if(j.ElementExtensions){a(Element.Methods,HTMLElement.prototype);a(Element.Methods.Simulated,HTMLElement.prototype,true)}if(j.SpecificElementExtensions){for(var k in Element.Methods.ByTag){var g=f(k);if(Object.isUndefined(g)){continue}a(e[k],g.prototype)}}Object.extend(Element,Element.Methods);delete Element.ByTag;if(Element.extend.refresh){Element.extend.refresh()}Element.cache={}};document.viewport={getDimensions:function(){var a={},c=Prototype.Browser;$w("width height").each(function(f){var e=f.capitalize();if(c.WebKit&&!document.evaluate){a[f]=self["inner"+e]}else{if(c.Opera&&parseFloat(window.opera.version())<9.5){a[f]=document.body["client"+e]}else{a[f]=document.documentElement["client"+e]}}});return a},getWidth:function(){return this.getDimensions().width},getHeight:function(){return this.getDimensions().height},getScrollOffsets:function(){return Element._returnOffset(window.pageXOffset||document.documentElement.scrollLeft||document.body.scrollLeft,window.pageYOffset||document.documentElement.scrollTop||document.body.scrollTop)}};var Selector=Class.create({initialize:function(a){this.expression=a.strip();if(this.shouldUseSelectorsAPI()){this.mode="selectorsAPI"}else{if(this.shouldUseXPath()){this.mode="xpath";this.compileXPathMatcher()}else{this.mode="normal";this.compileMatcher()}}},shouldUseXPath:function(){if(!Prototype.BrowserFeatures.XPath){return false}var a=this.expression;if(Prototype.Browser.WebKit&&(a.include("-of-type")||a.include(":empty"))){return false}if((/(\[[\w-]*?:|:checked)/).test(a)){return false}return true},shouldUseSelectorsAPI:function(){if(!Prototype.BrowserFeatures.SelectorsAPI){return false}if(!Selector._div){Selector._div=new Element("div")}try{Selector._div.querySelector(this.expression)}catch(a){return false}return true},compileMatcher:function(){var e=this.expression,ps=Selector.patterns,h=Selector.handlers,c=Selector.criteria,le,p,m;if(Selector._cache[e]){this.matcher=Selector._cache[e];return}this.matcher=["this.matcher = function(root) {","var r = root, h = Selector.handlers, c = false, n;"];while(e&&le!=e&&(/\S/).test(e)){le=e;for(var i in ps){p=ps[i];if(m=e.match(p)){this.matcher.push(Object.isFunction(c[i])?c[i](m):new Template(c[i]).evaluate(m));e=e.replace(m[0],"");break}}}this.matcher.push("return h.unique(n);\n}");eval(this.matcher.join("\n"));Selector._cache[this.expression]=this.matcher},compileXPathMatcher:function(){var g=this.expression,h=Selector.patterns,c=Selector.xpath,f,a;if(Selector._cache[g]){this.xpath=Selector._cache[g];return}this.matcher=[".//*"];while(g&&f!=g&&(/\S/).test(g)){f=g;for(var d in h){if(a=g.match(h[d])){this.matcher.push(Object.isFunction(c[d])?c[d](a):new Template(c[d]).evaluate(a));g=g.replace(a[0],"");break}}}this.xpath=this.matcher.join("");Selector._cache[this.expression]=this.xpath},findElements:function(a){a=a||document;var d=this.expression,c;switch(this.mode){case"selectorsAPI":if(a!==document){var f=a.id,g=$(a).identify();d="#"+g+" "+d}c=$A(a.querySelectorAll(d)).map(Element.extend);a.id=f;return c;case"xpath":return document._getElementsByXPath(this.xpath,a);default:return this.matcher(a)}},match:function(k){this.tokens=[];var r=this.expression,a=Selector.patterns,g=Selector.assertions;var c,f,h;while(r&&c!==r&&(/\S/).test(r)){c=r;for(var l in a){f=a[l];if(h=r.match(f)){if(g[l]){this.tokens.push([l,Object.clone(h)]);r=r.replace(h[0],"")}else{return this.findElements(document).include(k)}}}}var q=true,d,o;for(var l=0,j;j=this.tokens[l];l++){d=j[0],o=j[1];if(!Selector.assertions[d](k,o)){q=false;break}}return q},toString:function(){return this.expression},inspect:function(){return"#<Selector:"+this.expression.inspect()+">"}});Object.extend(Selector,{_cache:{},xpath:{descendant:"//*",child:"/*",adjacent:"/following-sibling::*[1]",laterSibling:"/following-sibling::*",tagName:function(a){if(a[1]=="*"){return""}return"[local-name()='"+a[1].toLowerCase()+"' or local-name()='"+a[1].toUpperCase()+"']"},className:"[contains(concat(' ', @class, ' '), ' #{1} ')]",id:"[@id='#{1}']",attrPresence:function(a){a[1]=a[1].toLowerCase();return new Template("[@#{1}]").evaluate(a)},attr:function(a){a[1]=a[1].toLowerCase();a[3]=a[5]||a[6];return new Template(Selector.xpath.operators[a[2]]).evaluate(a)},pseudo:function(a){var c=Selector.xpath.pseudos[a[1]];if(!c){return""}if(Object.isFunction(c)){return c(a)}return new Template(Selector.xpath.pseudos[a[1]]).evaluate(a)},operators:{"=":"[@#{1}='#{3}']","!=":"[@#{1}!='#{3}']","^=":"[starts-with(@#{1}, '#{3}')]","$=":"[substring(@#{1}, (string-length(@#{1}) - string-length('#{3}') + 1))='#{3}']","*=":"[contains(@#{1}, '#{3}')]","~=":"[contains(concat(' ', @#{1}, ' '), ' #{3} ')]","|=":"[contains(concat('-', @#{1}, '-'), '-#{3}-')]"},pseudos:{"first-child":"[not(preceding-sibling::*)]","last-child":"[not(following-sibling::*)]","only-child":"[not(preceding-sibling::* or following-sibling::*)]",empty:"[count(*) = 0 and (count(text()) = 0)]",checked:"[@checked]",disabled:"[(@disabled) and (@type!='hidden')]",enabled:"[not(@disabled) and (@type!='hidden')]",not:function(c){var k=c[6],j=Selector.patterns,a=Selector.xpath,g,d;var h=[];while(k&&g!=k&&(/\S/).test(k)){g=k;for(var f in j){if(c=k.match(j[f])){d=Object.isFunction(a[f])?a[f](c):new Template(a[f]).evaluate(c);h.push("("+d.substring(1,d.length-1)+")");k=k.replace(c[0],"");break}}}return"[not("+h.join(" and ")+")]"},"nth-child":function(a){return Selector.xpath.pseudos.nth("(count(./preceding-sibling::*) + 1) ",a)},"nth-last-child":function(a){return Selector.xpath.pseudos.nth("(count(./following-sibling::*) + 1) ",a)},"nth-of-type":function(a){return Selector.xpath.pseudos.nth("position() ",a)},"nth-last-of-type":function(a){return Selector.xpath.pseudos.nth("(last() + 1 - position()) ",a)},"first-of-type":function(a){a[6]="1";return Selector.xpath.pseudos["nth-of-type"](a)},"last-of-type":function(a){a[6]="1";return Selector.xpath.pseudos["nth-last-of-type"](a)},"only-of-type":function(a){var c=Selector.xpath.pseudos;return c["first-of-type"](a)+c["last-of-type"](a)},nth:function(g,e){var h,j=e[6],d;if(j=="even"){j="2n+0"}if(j=="odd"){j="2n+1"}if(h=j.match(/^(\d+)$/)){return"["+g+"= "+h[1]+"]"}if(h=j.match(/^(-?\d*)?n(([+-])(\d+))?/)){if(h[1]=="-"){h[1]=-1}var f=h[1]?Number(h[1]):1;var c=h[2]?Number(h[2]):0;d="[((#{fragment} - #{b}) mod #{a} = 0) and ((#{fragment} - #{b}) div #{a} >= 0)]";return new Template(d).evaluate({fragment:g,a:f,b:c})}}}},criteria:{tagName:'n = h.tagName(n, r, "#{1}", c);      c = false;',className:'n = h.className(n, r, "#{1}", c);    c = false;',id:'n = h.id(n, r, "#{1}", c);           c = false;',attrPresence:'n = h.attrPresence(n, r, "#{1}", c); c = false;',attr:function(a){a[3]=(a[5]||a[6]);return new Template('n = h.attr(n, r, "#{1}", "#{3}", "#{2}", c); c = false;').evaluate(a)},pseudo:function(a){if(a[6]){a[6]=a[6].replace(/"/g,'\\"')}return new Template('n = h.pseudo(n, "#{1}", "#{6}", r, c); c = false;').evaluate(a)},descendant:'c = "descendant";',child:'c = "child";',adjacent:'c = "adjacent";',laterSibling:'c = "laterSibling";'},patterns:{laterSibling:/^\s*~\s*/,child:/^\s*>\s*/,adjacent:/^\s*\+\s*/,descendant:/^\s/,tagName:/^\s*(\*|[\w\-]+)(\b|$)?/,id:/^#([\w\-\*]+)(\b|$)/,className:/^\.([\w\-\*]+)(\b|$)/,pseudo:/^:((first|last|nth|nth-last|only)(-child|-of-type)|empty|checked|(en|dis)abled|not)(\((.*?)\))?(\b|$|(?=\s|[:+~>]))/,attrPresence:/^\[((?:[\w]+:)?[\w]+)\]/,attr:/\[((?:[\w-]*:)?[\w-]+)\s*(?:([!^$*~|]?=)\s*((['"])([^\4]*?)\4|([^'"][^\]]*?)))?\]/},assertions:{tagName:function(a,c){return c[1].toUpperCase()==a.tagName.toUpperCase()},className:function(a,c){return Element.hasClassName(a,c[1])},id:function(a,c){return a.id===c[1]},attrPresence:function(a,c){return Element.hasAttribute(a,c[1])},attr:function(c,d){var a=Element.readAttribute(c,d[1]);return a&&Selector.operators[d[2]](a,d[5]||d[6])}},handlers:{concat:function(d,c){for(var e=0,f;f=c[e];e++){d.push(f)}return d},mark:function(a){var e=Prototype.emptyFunction;for(var c=0,d;d=a[c];c++){d._countedByPrototype=e}return a},unmark:function(a){for(var c=0,d;d=a[c];c++){d._countedByPrototype=undefined}return a},index:function(a,e,h){a._countedByPrototype=Prototype.emptyFunction;if(e){for(var c=a.childNodes,f=c.length-1,d=1;f>=0;f--){var g=c[f];if(g.nodeType==1&&(!h||g._countedByPrototype)){g.nodeIndex=d++}}}else{for(var f=0,d=1,c=a.childNodes;g=c[f];f++){if(g.nodeType==1&&(!h||g._countedByPrototype)){g.nodeIndex=d++}}}},unique:function(c){if(c.length==0){return c}var e=[],f;for(var d=0,a=c.length;d<a;d++){if(!(f=c[d])._countedByPrototype){f._countedByPrototype=Prototype.emptyFunction;e.push(Element.extend(f))}}return Selector.handlers.unmark(e)},descendant:function(a){var e=Selector.handlers;for(var d=0,c=[],f;f=a[d];d++){e.concat(c,f.getElementsByTagName("*"))}return c},child:function(a){var f=Selector.handlers;for(var e=0,d=[],g;g=a[e];e++){for(var c=0,k;k=g.childNodes[c];c++){if(k.nodeType==1&&k.tagName!="!"){d.push(k)}}}return d},adjacent:function(a){for(var d=0,c=[],f;f=a[d];d++){var e=this.nextElementSibling(f);if(e){c.push(e)}}return c},laterSibling:function(a){var e=Selector.handlers;for(var d=0,c=[],f;f=a[d];d++){e.concat(c,Element.nextSiblings(f))}return c},nextElementSibling:function(a){while(a=a.nextSibling){if(a.nodeType==1){return a}}return null},previousElementSibling:function(a){while(a=a.previousSibling){if(a.nodeType==1){return a}}return null},tagName:function(a,k,d,c){var l=d.toUpperCase();var f=[],j=Selector.handlers;if(a){if(c){if(c=="descendant"){for(var g=0,e;e=a[g];g++){j.concat(f,e.getElementsByTagName(d))}return f}else{a=this[c](a)}if(d=="*"){return a}}for(var g=0,e;e=a[g];g++){if(e.tagName.toUpperCase()===l){f.push(e)}}return f}else{return k.getElementsByTagName(d)}},id:function(c,a,k,g){var j=$(k),e=Selector.handlers;if(!j){return[]}if(!c&&a==document){return[j]}if(c){if(g){if(g=="child"){for(var d=0,f;f=c[d];d++){if(j.parentNode==f){return[j]}}}else{if(g=="descendant"){for(var d=0,f;f=c[d];d++){if(Element.descendantOf(j,f)){return[j]}}}else{if(g=="adjacent"){for(var d=0,f;f=c[d];d++){if(Selector.handlers.previousElementSibling(j)==f){return[j]}}}else{c=e[g](c)}}}}for(var d=0,f;f=c[d];d++){if(f==j){return[j]}}return[]}return(j&&Element.descendantOf(j,a))?[j]:[]},className:function(c,a,d,e){if(c&&e){c=this[e](c)}return Selector.handlers.byClassName(c,a,d)},byClassName:function(d,c,g){if(!d){d=Selector.handlers.descendant([c])}var j=" "+g+" ";for(var f=0,e=[],h,a;h=d[f];f++){a=h.className;if(a.length==0){continue}if(a==g||(" "+a+" ").include(j)){e.push(h)}}return e},attrPresence:function(d,c,a,h){if(!d){d=c.getElementsByTagName("*")}if(d&&h){d=this[h](d)}var f=[];for(var e=0,g;g=d[e];e++){if(Element.hasAttribute(g,a)){f.push(g)}}return f},attr:function(a,k,j,l,d,c){if(!a){a=k.getElementsByTagName("*")}if(a&&c){a=this[c](a)}var m=Selector.operators[d],g=[];for(var f=0,e;e=a[f];f++){var h=Element.readAttribute(e,j);if(h===null){continue}if(m(h,l)){g.push(e)}}return g},pseudo:function(c,d,f,a,e){if(c&&e){c=this[e](c)}if(!c){c=a.getElementsByTagName("*")}return Selector.pseudos[d](c,f,a)}},pseudos:{"first-child":function(c,g,a){for(var e=0,d=[],f;f=c[e];e++){if(Selector.handlers.previousElementSibling(f)){continue}d.push(f)}return d},"last-child":function(c,g,a){for(var e=0,d=[],f;f=c[e];e++){if(Selector.handlers.nextElementSibling(f)){continue}d.push(f)}return d},"only-child":function(c,j,a){var f=Selector.handlers;for(var e=0,d=[],g;g=c[e];e++){if(!f.previousElementSibling(g)&&!f.nextElementSibling(g)){d.push(g)}}return d},"nth-child":function(c,d,a){return Selector.pseudos.nth(c,d,a)},"nth-last-child":function(c,d,a){return Selector.pseudos.nth(c,d,a,true)},"nth-of-type":function(c,d,a){return Selector.pseudos.nth(c,d,a,false,true)},"nth-last-of-type":function(c,d,a){return Selector.pseudos.nth(c,d,a,true,true)},"first-of-type":function(c,d,a){return Selector.pseudos.nth(c,"1",a,false,true)},"last-of-type":function(c,d,a){return Selector.pseudos.nth(c,"1",a,true,true)},"only-of-type":function(c,e,a){var d=Selector.pseudos;return d["last-of-type"](d["first-of-type"](c,e,a),e,a)},getIndices:function(d,c,e){if(d==0){return c>0?[c]:[]}return $R(1,e).inject([],function(a,f){if(0==(f-c)%d&&(f-c)/d>=0){a.push(f)}return a})},nth:function(c,u,w,t,e){if(c.length==0){return[]}if(u=="even"){u="2n+0"}if(u=="odd"){u="2n+1"}var s=Selector.handlers,r=[],d=[],g;s.mark(c);for(var q=0,f;f=c[q];q++){if(!f.parentNode._countedByPrototype){s.index(f.parentNode,t,e);d.push(f.parentNode)}}if(u.match(/^\d+$/)){u=Number(u);for(var q=0,f;f=c[q];q++){if(f.nodeIndex==u){r.push(f)}}}else{if(g=u.match(/^(-?\d*)?n(([+-])(\d+))?/)){if(g[1]=="-"){g[1]=-1}var x=g[1]?Number(g[1]):1;var v=g[2]?Number(g[2]):0;var y=Selector.pseudos.getIndices(x,v,c.length);for(var q=0,f,k=y.length;f=c[q];q++){for(var o=0;o<k;o++){if(f.nodeIndex==y[o]){r.push(f)}}}}}s.unmark(c);s.unmark(d);return r},empty:function(c,g,a){for(var e=0,d=[],f;f=c[e];e++){if(f.tagName=="!"||f.firstChild){continue}d.push(f)}return d},not:function(a,e,l){var j=Selector.handlers,o,d;var k=new Selector(e).findElements(l);j.mark(k);for(var g=0,f=[],c;c=a[g];g++){if(!c._countedByPrototype){f.push(c)}}j.unmark(k);return f},enabled:function(c,g,a){for(var e=0,d=[],f;f=c[e];e++){if(!f.disabled&&(!f.type||f.type!=="hidden")){d.push(f)}}return d},disabled:function(c,g,a){for(var e=0,d=[],f;f=c[e];e++){if(f.disabled){d.push(f)}}return d},checked:function(c,g,a){for(var e=0,d=[],f;f=c[e];e++){if(f.checked){d.push(f)}}return d}},operators:{"=":function(c,a){return c==a},"!=":function(c,a){return c!=a},"^=":function(c,a){return c==a||c&&c.startsWith(a)},"$=":function(c,a){return c==a||c&&c.endsWith(a)},"*=":function(c,a){return c==a||c&&c.include(a)},"$=":function(c,a){return c.endsWith(a)},"*=":function(c,a){return c.include(a)},"~=":function(c,a){return(" "+c+" ").include(" "+a+" ")},"|=":function(c,a){return("-"+(c||"").toUpperCase()+"-").include("-"+(a||"").toUpperCase()+"-")}},split:function(c){var a=[];c.scan(/(([\w#:.~>+()\s-]+|\*|\[.*?\])+)\s*(,|$)/,function(d){a.push(d[1].strip())});return a},matchElements:function(g,j){var f=$$(j),e=Selector.handlers;e.mark(f);for(var d=0,c=[],a;a=g[d];d++){if(a._countedByPrototype){c.push(a)}}e.unmark(f);return c},findElement:function(c,d,a){if(Object.isNumber(d)){a=d;d=false}return Selector.matchElements(c,d||"*")[a||0]},findChildElements:function(f,j){j=Selector.split(j.join(","));var e=[],g=Selector.handlers;for(var d=0,c=j.length,a;d<c;d++){a=new Selector(j[d].strip());g.concat(e,a.findElements(f))}return(c>1)?g.unique(e):e}});if(Prototype.Browser.IE){Object.extend(Selector.handlers,{concat:function(d,c){for(var e=0,f;f=c[e];e++){if(f.tagName!=="!"){d.push(f)}}return d},unmark:function(a){for(var c=0,d;d=a[c];c++){d.removeAttribute("_countedByPrototype")}return a}})}function $$(){return Selector.findChildElements(document,$A(arguments))}var Form={reset:function(a){$(a).reset();return a},serializeElements:function(h,c){if(typeof c!="object"){c={hash:!!c}}else{if(Object.isUndefined(c.hash)){c.hash=true}}var d,g,a=false,f=c.submit;var e=h.inject({},function(j,k){if(!k.disabled&&k.name){d=k.name;g=$(k).getValue();if(g!=null&&k.type!="file"&&(k.type!="submit"||(!a&&f!==false&&(!f||d==f)&&(a=true)))){if(d in j){if(!Object.isArray(j[d])){j[d]=[j[d]]}j[d].push(g)}else{j[d]=g}}}return j});return c.hash?e:Object.toQueryString(e)}};Form.Methods={serialize:function(c,a){return Form.serializeElements(Form.getElements(c),a)},getElements:function(a){return $A($(a).getElementsByTagName("*")).inject([],function(c,d){if(Form.Element.Serializers[d.tagName.toLowerCase()]){c.push(Element.extend(d))}return c})},getInputs:function(h,d,e){h=$(h);var a=h.getElementsByTagName("input");if(!d&&!e){return $A(a).map(Element.extend)}for(var f=0,j=[],g=a.length;f<g;f++){var c=a[f];if((d&&c.type!=d)||(e&&c.name!=e)){continue}j.push(Element.extend(c))}return j},disable:function(a){a=$(a);Form.getElements(a).invoke("disable");return a},enable:function(a){a=$(a);Form.getElements(a).invoke("enable");return a},findFirstElement:function(c){var d=$(c).getElements().findAll(function(e){return"hidden"!=e.type&&!e.disabled});var a=d.findAll(function(e){return e.hasAttribute("tabIndex")&&e.tabIndex>=0}).sortBy(function(e){return e.tabIndex}).first();return a?a:d.find(function(e){return["input","select","textarea"].include(e.tagName.toLowerCase())})},focusFirstElement:function(a){a=$(a);a.findFirstElement().activate();return a},request:function(c,a){c=$(c),a=Object.clone(a||{});var e=a.parameters,d=c.readAttribute("action")||"";if(d.blank()){d=window.location.href}a.parameters=c.serialize(true);if(e){if(Object.isString(e)){e=e.toQueryParams()}Object.extend(a.parameters,e)}if(c.hasAttribute("method")&&!a.method){a.method=c.method}return new Ajax.Request(d,a)}};Form.Element={focus:function(a){$(a).focus();return a},select:function(a){$(a).select();return a}};Form.Element.Methods={serialize:function(a){a=$(a);if(!a.disabled&&a.name){var c=a.getValue();if(c!=undefined){var d={};d[a.name]=c;return Object.toQueryString(d)}}return""},getValue:function(a){a=$(a);var c=a.tagName.toLowerCase();return Form.Element.Serializers[c](a)},setValue:function(a,c){a=$(a);var d=a.tagName.toLowerCase();Form.Element.Serializers[d](a,c);return a},clear:function(a){$(a).value="";return a},present:function(a){return $(a).value!=""},activate:function(a){a=$(a);try{a.focus();if(a.select&&(a.tagName.toLowerCase()!="input"||!["button","reset","submit"].include(a.type))){a.select()}}catch(c){}return a},disable:function(a){a=$(a);a.disabled=true;return a},enable:function(a){a=$(a);a.disabled=false;return a}};var Field=Form.Element;var $F=Form.Element.Methods.getValue;Form.Element.Serializers={input:function(a,c){switch(a.type.toLowerCase()){case"checkbox":case"radio":return Form.Element.Serializers.inputSelector(a,c);default:return Form.Element.Serializers.textarea(a,c)}},inputSelector:function(a,c){if(Object.isUndefined(c)){return a.checked?a.value:null}else{a.checked=!!c}},textarea:function(a,c){if(Object.isUndefined(c)){return a.value}else{a.value=c}},select:function(d,g){if(Object.isUndefined(g)){return this[d.type=="select-one"?"selectOne":"selectMany"](d)}else{var c,e,h=!Object.isArray(g);for(var a=0,f=d.length;a<f;a++){c=d.options[a];e=this.optionValue(c);if(h){if(e==g){c.selected=true;return}}else{c.selected=g.include(e)}}}},selectOne:function(c){var a=c.selectedIndex;return a>=0?this.optionValue(c.options[a]):null},selectMany:function(e){var a,f=e.length;if(!f){return null}for(var d=0,a=[];d<f;d++){var c=e.options[d];if(c.selected){a.push(this.optionValue(c))}}return a},optionValue:function(a){return Element.extend(a).hasAttribute("value")?a.value:a.text}};Abstract.TimedObserver=Class.create(PeriodicalExecuter,{initialize:function($super,a,c,d){$super(d,c);this.element=$(a);this.lastValue=this.getValue()},execute:function(){var a=this.getValue();if(Object.isString(this.lastValue)&&Object.isString(a)?this.lastValue!=a:String(this.lastValue)!=String(a)){this.callback(this.element,a);this.lastValue=a}}});Form.Element.Observer=Class.create(Abstract.TimedObserver,{getValue:function(){return Form.Element.getValue(this.element)}});Form.Observer=Class.create(Abstract.TimedObserver,{getValue:function(){return Form.serialize(this.element)}});Abstract.EventObserver=Class.create({initialize:function(a,c){this.element=$(a);this.callback=c;this.lastValue=this.getValue();if(this.element.tagName.toLowerCase()=="form"){this.registerFormCallbacks()}else{this.registerCallback(this.element)}},onElementEvent:function(){var a=this.getValue();if(this.lastValue!=a){this.callback(this.element,a);this.lastValue=a}},registerFormCallbacks:function(){Form.getElements(this.element).each(this.registerCallback,this)},registerCallback:function(a){if(a.type){switch(a.type.toLowerCase()){case"checkbox":case"radio":Event.observe(a,"click",this.onElementEvent.bind(this));break;default:Event.observe(a,"change",this.onElementEvent.bind(this));break}}}});Form.Element.EventObserver=Class.create(Abstract.EventObserver,{getValue:function(){return Form.Element.getValue(this.element)}});Form.EventObserver=Class.create(Abstract.EventObserver,{getValue:function(){return Form.serialize(this.element)}});if(!window.Event){var Event={}}Object.extend(Event,{KEY_BACKSPACE:8,KEY_TAB:9,KEY_RETURN:13,KEY_ESC:27,KEY_LEFT:37,KEY_UP:38,KEY_RIGHT:39,KEY_DOWN:40,KEY_DELETE:46,KEY_HOME:36,KEY_END:35,KEY_PAGEUP:33,KEY_PAGEDOWN:34,KEY_INSERT:45,cache:{},relatedTarget:function(c){var a;switch(c.type){case"mouseover":a=c.fromElement;break;case"mouseout":a=c.toElement;break;default:return null}return Element.extend(a)}});Event.Methods=(function(){var a;if(Prototype.Browser.IE){var c={0:1,1:4,2:2};a=function(e,d){return e.button==c[d]}}else{if(Prototype.Browser.WebKit){a=function(e,d){switch(d){case 0:return e.which==1&&!e.metaKey;case 1:return e.which==1&&e.metaKey;default:return false}}}else{a=function(e,d){return e.which?(e.which===d+1):(e.button===d)}}}return{isLeftClick:function(d){return a(d,0)},isMiddleClick:function(d){return a(d,1)},isRightClick:function(d){return a(d,2)},element:function(f){f=Event.extend(f);var e=f.target,d=f.type,g=f.currentTarget;if(g&&g.tagName){if(d==="load"||d==="error"||(d==="click"&&g.tagName.toLowerCase()==="input"&&g.type==="radio")){e=g}}if(e.nodeType==Node.TEXT_NODE){e=e.parentNode}return Element.extend(e)},findElement:function(e,g){var d=Event.element(e);if(!g){return d}var f=[d].concat(d.ancestors());return Selector.findElement(f,g,0)},pointer:function(f){var e=document.documentElement,d=document.body||{scrollLeft:0,scrollTop:0};return{x:f.pageX||(f.clientX+(e.scrollLeft||d.scrollLeft)-(e.clientLeft||0)),y:f.pageY||(f.clientY+(e.scrollTop||d.scrollTop)-(e.clientTop||0))}},pointerX:function(d){return Event.pointer(d).x},pointerY:function(d){return Event.pointer(d).y},stop:function(d){Event.extend(d);d.preventDefault();d.stopPropagation();d.stopped=true}}})();Event.extend=(function(){var a=Object.keys(Event.Methods).inject({},function(c,d){c[d]=Event.Methods[d].methodize();return c});if(Prototype.Browser.IE){Object.extend(a,{stopPropagation:function(){this.cancelBubble=true},preventDefault:function(){this.returnValue=false},inspect:function(){return"[object Event]"}});return function(c){if(!c){return false}if(c._extendedByPrototype){return c}c._extendedByPrototype=Prototype.emptyFunction;var d=Event.pointer(c);Object.extend(c,{target:c.srcElement,relatedTarget:Event.relatedTarget(c),pageX:d.x,pageY:d.y});return Object.extend(c,a)}}else{Event.prototype=Event.prototype||document.createEvent("HTMLEvents")["__proto__"];Object.extend(Event.prototype,a);return Prototype.K}})();Object.extend(Event,(function(){var c=Event.cache;function d(l){if(l._prototypeEventID){return l._prototypeEventID[0]}arguments.callee.id=arguments.callee.id||1;return l._prototypeEventID=[++arguments.callee.id]}function h(l){if(l&&l.include(":")){return"dataavailable"}return l}function a(l){return c[l]=c[l]||{}}function g(o,l){var m=a(o);return m[l]=m[l]||[]}function j(m,l,o){var s=d(m);var r=g(s,l);if(r.pluck("handler").include(o)){return false}var q=function(t){if(!Event||!Event.extend||(t.eventName&&t.eventName!=l)){return false}Event.extend(t);o.call(m,t)};q.handler=o;r.push(q);return q}function k(q,l,m){var o=g(q,l);return o.find(function(r){return r.handler==m})}function e(q,l,m){var o=a(q);if(!o[l]){return false}o[l]=o[l].without(k(q,l,m))}function f(){for(var m in c){for(var l in c[m]){c[m][l]=null}}}if(window.attachEvent){window.attachEvent("onunload",f)}if(Prototype.Browser.WebKit){window.addEventListener("unload",Prototype.emptyFunction,false)}return{observe:function(o,l,q){o=$(o);var m=h(l);var r=j(o,l,q);if(!r){return o}if(o.addEventListener){o.addEventListener(m,r,false)}else{o.attachEvent("on"+m,r)}return o},stopObserving:function(o,l,q){o=$(o);var s=d(o),m=h(l);if(!q&&l){g(s,l).each(function(t){o.stopObserving(l,t.handler)});return o}else{if(!l){Object.keys(a(s)).each(function(t){o.stopObserving(t)});return o}}var r=k(s,l,q);if(!r){return o}if(o.removeEventListener){o.removeEventListener(m,r,false)}else{o.detachEvent("on"+m,r)}e(s,l,q);return o},fire:function(o,m,l){o=$(o);if(o==document&&document.createEvent&&!o.dispatchEvent){o=document.documentElement}var q;if(document.createEvent){q=document.createEvent("HTMLEvents");q.initEvent("dataavailable",true,true)}else{q=document.createEventObject();q.eventType="ondataavailable"}q.eventName=m;q.memo=l||{};if(document.createEvent){o.dispatchEvent(q)}else{o.fireEvent(q.eventType,q)}return Event.extend(q)}}})());Object.extend(Event,Event.Methods);Element.addMethods({fire:Event.fire,observe:Event.observe,stopObserving:Event.stopObserving});Object.extend(document,{fire:Element.Methods.fire.methodize(),observe:Element.Methods.observe.methodize(),stopObserving:Element.Methods.stopObserving.methodize(),loaded:false});(function(){var c;function a(){if(document.loaded){return}if(c){window.clearInterval(c)}document.fire("dom:loaded");document.loaded=true}if(document.addEventListener){if(Prototype.Browser.WebKit){c=window.setInterval(function(){if(/loaded|complete/.test(document.readyState)){a()}},0);Event.observe(window,"load",a)}else{document.addEventListener("DOMContentLoaded",a,false)}}else{document.write("<script id=__onDOMContentLoaded defer src=//:><\/script>");$("__onDOMContentLoaded").onreadystatechange=function(){if(this.readyState=="complete"){this.onreadystatechange=null;a()}}}})();Hash.toQueryString=Object.toQueryString;var Toggle={display:Element.toggle};Element.Methods.childOf=Element.Methods.descendantOf;var Insertion={Before:function(a,c){return Element.insert(a,{before:c})},Top:function(a,c){return Element.insert(a,{top:c})},Bottom:function(a,c){return Element.insert(a,{bottom:c})},After:function(a,c){return Element.insert(a,{after:c})}};var $continue=new Error('"throw $continue" is deprecated, use "return" instead');var Position={includeScrollOffsets:false,prepare:function(){this.deltaX=window.pageXOffset||document.documentElement.scrollLeft||document.body.scrollLeft||0;this.deltaY=window.pageYOffset||document.documentElement.scrollTop||document.body.scrollTop||0},within:function(c,a,d){if(this.includeScrollOffsets){return this.withinIncludingScrolloffsets(c,a,d)}this.xcomp=a;this.ycomp=d;this.offset=Element.cumulativeOffset(c);return(d>=this.offset[1]&&d<this.offset[1]+c.offsetHeight&&a>=this.offset[0]&&a<this.offset[0]+c.offsetWidth)},withinIncludingScrolloffsets:function(c,a,e){var d=Element.cumulativeScrollOffset(c);this.xcomp=a+d[0]-this.deltaX;this.ycomp=e+d[1]-this.deltaY;this.offset=Element.cumulativeOffset(c);return(this.ycomp>=this.offset[1]&&this.ycomp<this.offset[1]+c.offsetHeight&&this.xcomp>=this.offset[0]&&this.xcomp<this.offset[0]+c.offsetWidth)},overlap:function(c,a){if(!c){return 0}if(c=="vertical"){return((this.offset[1]+a.offsetHeight)-this.ycomp)/a.offsetHeight}if(c=="horizontal"){return((this.offset[0]+a.offsetWidth)-this.xcomp)/a.offsetWidth}},cumulativeOffset:Element.Methods.cumulativeOffset,positionedOffset:Element.Methods.positionedOffset,absolutize:function(a){Position.prepare();return Element.absolutize(a)},relativize:function(a){Position.prepare();return Element.relativize(a)},realOffset:Element.Methods.cumulativeScrollOffset,offsetParent:Element.Methods.getOffsetParent,page:Element.Methods.viewportOffset,clone:function(c,d,a){a=a||{};return Element.clonePosition(d,c,a)}};if(!document.getElementsByClassName){document.getElementsByClassName=function(c){function a(d){return d.blank()?null:"[contains(concat(' ', @class, ' '), ' "+d+" ')]"}c.getElementsByClassName=Prototype.BrowserFeatures.XPath?function(d,f){f=f.toString().strip();var e=/\s/.test(f)?$w(f).map(a).join(""):a(f);return e?document._getElementsByXPath(".//*"+e,d):[]}:function(f,g){g=g.toString().strip();var h=[],j=(/\s/.test(g)?$w(g):null);if(!j&&!g){return h}var d=$(f).getElementsByTagName("*");g=" "+g+" ";for(var e=0,l,k;l=d[e];e++){if(l.className&&(k=" "+l.className+" ")&&(k.include(g)||(j&&j.all(function(m){return !m.toString().blank()&&k.include(" "+m+" ")})))){h.push(Element.extend(l))}}return h};return function(e,d){return $(d||document.body).getElementsByClassName(e)}}(Element.Methods)}Element.ClassNames=Class.create();Element.ClassNames.prototype={initialize:function(a){this.element=$(a)},_each:function(a){this.element.className.split(/\s+/).select(function(c){return c.length>0})._each(a)},set:function(a){this.element.className=a},add:function(a){if(this.include(a)){return}this.set($A(this).concat(a).join(" "))},remove:function(a){if(!this.include(a)){return}this.set($A(this).without(a).join(" "))},toString:function(){return $A(this).join(" ")}};Object.extend(Element.ClassNames.prototype,Enumerable);Element.addMethods();if(typeof(Console)==="undefined"){}function puts(c,a){Console.puts(c,a)}function p(){Console.p.apply(this,arguments)}BiwaScheme.TopEnv={};BiwaScheme.CoreEnv={};BiwaScheme.Error=Class.create({initialize:function(a){this.message="Error: "+a},toString:function(){return this.message}});BiwaScheme.Bug=Class.create(Object.extend(new BiwaScheme.Error(),{initialize:function(a){this.message="[BUG] "+a}}));BiwaScheme.UserError=Class.create(Object.extend(new BiwaScheme.Error(),{initialize:function(a){this.message=a}}));BiwaScheme.Set=Class.create({initialize:function(){this.arr=[];var a;for(a=0;a<arguments.length;a++){this.arr[a]=arguments[a]}},equals:function(c){if(this.arr.length!=c.arr.length){return false}var d=this.arr.clone();var a=c.arr.clone();d.sort();a.sort();for(var e=0;e<this.arr.length;e++){if(d[e]!=a[e]){return false}}return true},set_cons:function(a){var c=new BiwaScheme.Set(a);c.arr=this.arr.clone();c.arr.push(a);return c},set_union:function(){var e=new BiwaScheme.Set();e.arr=this.arr.clone();for(var a=0;a<arguments.length;a++){var c=arguments[a];if(!c instanceof BiwaScheme.Set){throw new BiwaScheme.Error("set_union: arguments must be a set")}for(var d=0;d<c.arr.length;d++){e.add(c.arr[d])}}return e},set_intersect:function(a){if(!a instanceof BiwaScheme.Set){throw new BiwaScheme.Error("set_intersect: arguments must be a set")}var d=new BiwaScheme.Set();for(var c=0;c<this.arr.length;c++){if(a.member(this.arr[c])){d.add(this.arr[c])}}return d},set_minus:function(a){if(!a instanceof BiwaScheme.Set){throw new BiwaScheme.Error("set_minus: arguments must be a set")}var d=new BiwaScheme.Set();for(var c=0;c<this.arr.length;c++){if(!a.member(this.arr[c])){d.add(this.arr[c])}}return d},add:function(a){if(!this.member(a)){this.arr.push(a)}},member:function(c){for(var a=0;a<this.arr.length;a++){if(this.arr[a]==c){return true}}return false},rindex:function(c){for(var a=this.arr.length-1;a>=0;a--){if(this.arr[a]==c){return(this.arr.length-1-a)}}return null},index:function(c){for(var a=0;a<this.arr.length;a++){if(this.arr[a]==c){return a}}return null},inspect:function(){return"Set("+this.arr.join(", ")+")"},toString:function(){return this.inspect()},size:function(){return this.arr.length}});Function.prototype.to_write=function(){return"#<Function "+(this.fname?this.fname:this.toSource?this.toSource().truncate(40):"")+">"};String.prototype.to_write=function(){return'"'+this.replace(/\\|\"/g,function(a){return"\\"+a}).replace(/\x07/g,"\\a").replace(/\x08/g,"\\b").replace(/\t/g,"\\t").replace(/\n/g,"\\n").replace(/\v/g,"\\v").replace(/\f/g,"\\f").replace(/\r/g,"\\r")+'"'};Array.prototype.to_write=function(){if(this.closure_p){return"#<Closure>"}var c=[];for(var d=0;d<this.length;d++){c.push(BiwaScheme.to_write(this[d]))}return"#("+c.join(" ")+")"};BiwaScheme.to_write=function(a){if(a===undefined){return"undefined"}else{if(a===null){return"null"}else{if(typeof(a.to_write)=="function"){return a.to_write()}else{if(isNaN(a)&&typeof(a)=="number"){return"+nan.0"}else{switch(a){case true:return"#t";case false:return"#f";case Infinity:return"+inf.0";case -Infinity:return"-inf.0"}}}}}return Object.inspect(a)};BiwaScheme.to_display=function(a){if(typeof(a.valueOf())=="string"){return a}else{if(a instanceof BiwaScheme.Symbol){return a.name}else{if(a instanceof Array){return"#("+a.map(BiwaScheme.to_display).join(" ")+")"}else{if(a instanceof BiwaScheme.Pair){return a.inspect(BiwaScheme.to_display)}else{if(a instanceof BiwaScheme.Char){return a.value}else{return BiwaScheme.to_write(a)}}}}}};BiwaScheme.write_ss=function(f,a){var e=[f],d=[false];BiwaScheme.find_cyclic(f,e,d);var h=BiwaScheme.reduce_cyclic_info(e,d);var g=new Array(h.length);for(var c=h.length-1;c>=0;c--){g[c]=false}return BiwaScheme.to_write_ss(f,h,g,a)};BiwaScheme.to_write_ss=function(g,k,j,c){var e="";var f=k.indexOf(g);if(f>=0){if(j[f]){return"#"+f+"#"}else{j[f]=true;e="#"+f+"="}}if(g instanceof BiwaScheme.Pair){var d=[];d.push(BiwaScheme.to_write_ss(g.car,k,j,c));for(var h=g.cdr;h!=BiwaScheme.nil;h=h.cdr){if(!(h instanceof BiwaScheme.Pair)||k.indexOf(h)>=0){d.push(".");d.push(BiwaScheme.to_write_ss(h,k,j,c));break}d.push(BiwaScheme.to_write_ss(h.car,k,j,c))}e+="("+d.join(" ")+")"}else{if(g instanceof Array){var d=g.map(function(a){return BiwaScheme.to_write_ss(a,k,j,c)});if(c){e+="["+d.join(", ")+"]"}else{e+="#("+d.join(" ")+")"}}else{e+=BiwaScheme.to_write(g)}}return e};BiwaScheme.reduce_cyclic_info=function(e,d){var c=0;for(var a=0;a<d.length;a++){if(d[a]){e[c]=e[a];c++}}return e.slice(0,c)};BiwaScheme.find_cyclic=function(e,d,c){var a=(e instanceof BiwaScheme.Pair)?[e.car,e.cdr]:(e instanceof Array)?e:null;if(!a){return}a.each(function(g){if(typeof(g)=="number"||typeof(g)=="string"||g===BiwaScheme.undef||g===true||g===false||g===BiwaScheme.nil||g instanceof BiwaScheme.Symbol){return}var f=d.indexOf(g);if(f>=0){c[f]=true}else{d.push(g);c.push(false);BiwaScheme.find_cyclic(g,d,c)}})};BiwaScheme.Pair=Class.create({initialize:function(a,c){this.car=a;this.cdr=c},caar:function(){return this.car.car},cadr:function(){return this.cdr.car},cdar:function(){return this.cdr.car},cddr:function(){return this.cdr.cdr},first:function(){return this.car},second:function(){return this.cdr.car},third:function(){return this.cdr.cdr.car},fourth:function(){return this.cdr.cdr.cdr.car},fifth:function(){return this.cdr.cdr.cdr.cdr.car},to_array:function(){var a=[];for(var c=this;c instanceof BiwaScheme.Pair;c=c.cdr){a.push(c.car)}return a},to_set:function(){var c=new BiwaScheme.Set();for(var a=this;a instanceof BiwaScheme.Pair;a=a.cdr){c.add(a.car)}return c},length:function(){var c=0;for(var a=this;a instanceof BiwaScheme.Pair;a=a.cdr){c++}return c},foreach:function(a){for(var c=this;c instanceof BiwaScheme.Pair;c=c.cdr){a(c.car)}return c},map:function(c){var a=[];for(var d=this;BiwaScheme.isPair(d);d=d.cdr){a.push(c(d.car))}return a},concat:function(a){var c=this;while(c instanceof BiwaScheme.Pair&&c.cdr!=BiwaScheme.nil){c=c.cdr}c.cdr=a;return this},inspect:function(e){e||(e=Object.inspect);var c=[];var d=this.foreach(function(a){c.push(e(a))});if(d!=BiwaScheme.nil){c.push(".");c.push(e(d))}return"("+c.join(" ")+")"},toString:function(){return this.inspect()},to_write:function(){return this.inspect(BiwaScheme.to_write)}});BiwaScheme.List=function(){return $A(arguments).to_list()};BiwaScheme.build_list=function(c){var d=BiwaScheme.nil;for(var a=c.length-1;a>=0;a--){var e=c[a];if(Object.isArray(e)&&!e.is_vector){e=BiwaScheme.build_list(e)}d=new BiwaScheme.Pair(e,d)}return d};Array.prototype.to_list=function(){var c=BiwaScheme.nil;for(var a=this.length-1;a>=0;a--){c=new BiwaScheme.Pair(this[a],c)}return c};BiwaScheme.Values=Class.create({initialize:function(a){this.content=a},to_write:function(){return"#<Values "+this.content.map(BiwaScheme.to_write).join(" ")+">"}});BiwaScheme.nil={toString:function(){return"nil"},to_write:function(){return"()"},to_array:function(){return[]},length:function(){return 0}};BiwaScheme.undef=new Object();BiwaScheme.undef.toString=function(){return"#<undef>"};BiwaScheme.eof=new Object;BiwaScheme.Symbol=Class.create({initialize:function(a){this.name=a;BiwaScheme.Symbols[a]=this},inspect:function(){return"'"+this.name},toString:function(){return"'"+this.name},to_write:function(){return this.name}});BiwaScheme.Symbols={};BiwaScheme.Sym=function(a,c){if(BiwaScheme.Symbols[a]===undefined){return new BiwaScheme.Symbol(a)}else{if(!(BiwaScheme.Symbols[a] instanceof BiwaScheme.Symbol)){return new BiwaScheme.Symbol(a)}else{return BiwaScheme.Symbols[a]}}};BiwaScheme.gensyms=0;BiwaScheme.gensym=function(){BiwaScheme.gensyms++;return BiwaScheme.Sym("__gensym_"+BiwaScheme.gensyms)};BiwaScheme.Char=Class.create({initialize:function(a){BiwaScheme.Chars[this.value=a]=this},to_write:function(){switch(this.value){case"\n":return"#\\newline";case" ":return"#\\space";case"\t":return"#\\tab";default:return"#\\"+this.value}},inspect:function(){return this.to_write()}});BiwaScheme.Chars={};BiwaScheme.Char.get=function(a){if(typeof(a)!="string"){throw new BiwaScheme.Bug("Char.get: "+Object.inspect(a)+" is not a string")}if(BiwaScheme.Chars[a]===undefined){return new BiwaScheme.Char(a)}else{return BiwaScheme.Chars[a]}};BiwaScheme.Complex=Class.create({initialize:function(c,a){this.real=c;this.imag=a},magnitude:function(){return Math.sqrt(this.real*this.real+this.imag*this.imag)},angle:function(){return Math.acos(this.real/this.magnitude())}});BiwaScheme.Complex.from_polar=function(c,a){var e=c*Math.cos(a);var d=c*Math.sin(a);return new BiwaScheme.Complex(e,d)};BiwaScheme.Complex.assure=function(a){if(a instanceof BiwaScheme.Complex){return a}else{return new BiwaScheme.Complex(a,0)}};BiwaScheme.Rational=Class.create({initialize:function(a,c){this.numerator=a;this.denominator=c}});BiwaScheme.Port=Class.create({initialize:function(a,c){this.is_open=true;this.is_binary=false;this.is_input=a;this.is_output=c},close:function(){this.is_open=false},inspect:function(){return"#<Port>"},to_write:function(){return"#<Port>"}});BiwaScheme.Port.BrowserInput=Class.create(BiwaScheme.Port,{initialize:function($super){$super(true,false)},get_string:function(c){var a=document.createElement("div");a.innerHTML="<input id='webscheme-read-line' type='text'><input id='webscheme-read-line-submit' type='button' value='ok'>";$("bs-console").appendChild(a);return new BiwaScheme.Pause(function(d){Event.observe($("webscheme-read-line-submit"),"click",function(){var e=$("webscheme-read-line").value;a.parentNode.removeChild(a);puts(e);d.resume(c(e))})})}});BiwaScheme.Port.DefaultOutput=Class.create(BiwaScheme.Port,{initialize:function($super){$super(false,true)},put_string:function(a){puts(a,true)}});BiwaScheme.Port.StringOutput=Class.create(BiwaScheme.Port,{initialize:function($super){this.buffer=[];$super(false,true)},put_string:function(a){this.buffer.push(a)},output_string:function(a){return this.buffer.join("")}});BiwaScheme.Port.StringInput=Class.create(BiwaScheme.Port,{initialize:function($super,a){this.str=a;$super(true,false)},get_string:function(a){return a(this.str)}});BiwaScheme.Port.current_input=new BiwaScheme.Port.BrowserInput();BiwaScheme.Port.current_output=new BiwaScheme.Port.DefaultOutput();BiwaScheme.Port.current_error=new BiwaScheme.Port.DefaultOutput();BiwaScheme.Record=Class.create({initialize:function(c,a){assert_record_td(c,"new Record");this.rtd=c;this.fields=a},get:function(a){return this.fields[a]},set:function(c,a){this.fields[c]=a},toString:function(){var a=BiwaScheme.to_write(this.fields);return"#<Record "+this.rtd.name+" "+a+">"}});BiwaScheme.isRecord=function(a){return(a instanceof BiwaScheme.Record)};BiwaScheme.Record._DefinedTypes=new Hash();BiwaScheme.Record.define_type=function(a,c,d){return BiwaScheme.Record._DefinedTypes.set(a,{rtd:c,cd:d})};BiwaScheme.Record.get_type=function(a){return BiwaScheme.Record._DefinedTypes.get(a)};BiwaScheme.Record.RTD=Class.create({initialize:function(e,d,f,c,g,a){this.name=e;this.parent_rtd=d;this.is_base_type=!d;if(f){this.uid=f;this.generative=false}else{this.uid=this._generate_new_uid();this.generative=true}this.sealed=!!c;this.opaque=d.opaque||(!!g);this.fields=a.map(function(h){return{name:h[0],mutable:!!h[1]}})},_generate_new_uid:function(){var a=(BiwaScheme.Record.RTD.last_uid++);return BiwaScheme.Sym("__record_td_uid_"+a)},toString:function(){return"#<RecordTD "+name+">"}});BiwaScheme.Record.RTD.last_uid=0;BiwaScheme.Record.RTD.NongenerativeRecords=new Hash();BiwaScheme.isRecordTD=function(a){return(a instanceof BiwaScheme.Record.RTD)};BiwaScheme.Record.CD=Class.create({initialize:function(a,c,d){this._check(a,c,d);this.rtd=a;this.parent_cd=c;if(d){this.has_custom_protocol=true;this.protocol=d}else{this.has_custom_protocol=false;if(a.parent_rtd){this.protocol=this._default_protocol_for_derived_types()}else{this.protocol=this._default_protocol_for_base_types()}}},_check:function(a,c,d){if(a.is_base_type&&c){throw new Error("Record.CD.new: cannot specify parent cd of a base type")}if(c&&a.parent_rtd&&(c.rtd!=a.parent_rtd)){throw new Error("Record.CD.new: mismatched parents between rtd and parent_cd")}if(a.parent_rtd&&!c&&d){throw new Error("Record.CD.new: protocol must be #f when parent_cd is not given")}if(c&&c.has_custom_protocol&&!d){throw new Error("Record.CD.new: protocol must be specified when parent_cd has a custom protocol")}},_default_protocol_for_base_types:function(){return function(a){var c=a[0];assert_procedure(c,"_default_protocol/base");return c}},_default_protocol_for_derived_types:function(){var a=this.rtd;return function(c){var e=c[0];assert_procedure(e,"_default_protocol/n");var d=function(g){var j=a.fields.length;var k=g.length-j;var f=g.slice(0,k);var h=g.slice(k);return new BiwaScheme.Call(e,f,function(l){var m=l[0];assert_procedure(m,"_default_protocol/p");return new BiwaScheme.Call(m,h,function(q){var o=q[0];assert_record(o,"_default_protocol/result");return o})})};return d}},toString:function(){return"#<RecordCD "+this.rtd.name+">"},record_constructor:function(){var a=(this.parent_cd?this._make_n([],this.rtd):this._make_p()).bind(this);return new BiwaScheme.Call(this.protocol,[a],function(c){var d=c[0];assert_procedure(d,"record_constructor");return d})},_make_p:function(){return function(a){return new BiwaScheme.Record(this.rtd,a)}},_make_n:function(a,c){var d=this.parent_cd;if(d){var e=function(g){var f=function(k){var j=[].concat(k[0]).concat(a);var h=d._make_n(j,c);return new BiwaScheme.Call(d.protocol,[h],function(l){var m=l[0];assert_procedure(m,"_make_n");return new BiwaScheme.Call(m,g,function(q){var o=q[0];assert_record(o);return o})})};return f};return e}else{var e=function(g){var f=g.concat(a);return new BiwaScheme.Record(c,f)};return e}}});BiwaScheme.isRecordCD=function(a){return(a instanceof BiwaScheme.Record.CD)};BiwaScheme.Hashtable=Class.create({initialize:function(c,d,a){this.mutable=(a===undefined)?true:a?true:false;this.hash_proc=c;this.equiv_proc=d;this.pairs_of=new Hash()},clear:function(){this.pairs_of=new Hash()},candidate_pairs:function(a){return this.pairs_of.get(a)},add_pair:function(e,a,d){var c=this.pairs_of.get(e);if(c){c.push([a,d])}else{this.pairs_of.set(e,[[a,d]])}},remove_pair:function(d,e){var c=this.pairs_of.get(d);var a=c.indexOf(e);if(a==-1){throw new BiwaScheme.Bug("Hashtable#remove_pair: pair not found!")}else{c.splice(a,1)}},create_copy:function(a){var c=new BiwaScheme.Hashtable(this.hash_proc,this.equiv_proc,a);this.pairs_of.each(function(e){var d=e[1].map(function(f){return f.clone()});c.pairs_of.set(e[0],d)});return c},size:function(){var a=0;this._apply_pair(function(c){a++});return a},keys:function(){return this._apply_pair(function(a){return a[0]})},values:function(){return this._apply_pair(function(a){return a[1]})},_apply_pair:function(d){var c=[];this.pairs_of.values().each(function(a){a.each(function(e){c.push(d(e))})});return c},to_write:function(){return"#<Hashtable size="+this.size()+">"}});BiwaScheme.Hashtable.equal_hash=function(a){return BiwaScheme.to_write(a[0])};BiwaScheme.Hashtable.eq_hash=BiwaScheme.Hashtable.equal_hash;BiwaScheme.Hashtable.eqv_hash=BiwaScheme.Hashtable.equal_hash;BiwaScheme.Hashtable.string_hash=function(a){return a[0]};BiwaScheme.Hashtable.string_ci_hash=function(a){return Object.isString(a[0])?a[0].toLowerCase():a[0]};BiwaScheme.Hashtable.symbol_hash=function(a){return(a[0] instanceof BiwaScheme.Symbol)?a[0].name:a[0]};BiwaScheme.Hashtable.eq_equiv=function(a){return BiwaScheme.eq(a[0],a[1])};BiwaScheme.Hashtable.eqv_equiv=function(a){return BiwaScheme.eqv(a[0],a[1])};BiwaScheme.Syntax=Class.create({initialize:function(a,c){this.sname=a;this.func=c},transform:function(a){if(!this.func){throw new BiwaScheme.Bug("sorry, syntax "+this.sname+" is a pseudo syntax now")}return this.func(a)},inspect:function(){return"#<Syntax "+this.sname+">"}});BiwaScheme.TopEnv.define=new BiwaScheme.Syntax("define");BiwaScheme.TopEnv.begin=new BiwaScheme.Syntax("begin");BiwaScheme.TopEnv.quote=new BiwaScheme.Syntax("quote");BiwaScheme.TopEnv.lambda=new BiwaScheme.Syntax("lambda");BiwaScheme.TopEnv["if"]=new BiwaScheme.Syntax("if");BiwaScheme.TopEnv["set!"]=new BiwaScheme.Syntax("set!");BiwaScheme.isNil=function(a){return(a===BiwaScheme.nil)};BiwaScheme.isUndef=function(a){return(a===BiwaScheme.undef)};BiwaScheme.isChar=function(a){return(a instanceof BiwaScheme.Char)};BiwaScheme.isSymbol=function(a){return(a instanceof BiwaScheme.Symbol)};BiwaScheme.isPort=function(a){return(a instanceof BiwaScheme.Port)};BiwaScheme.isPair=function(a){return(a instanceof BiwaScheme.Pair)};BiwaScheme.isList=function(a){if(a===BiwaScheme.nil){return true}if(!(a instanceof BiwaScheme.Pair)){return false}return BiwaScheme.isList(a.cdr)};BiwaScheme.isVector=function(a){return(a instanceof Array)&&(a.closure_p!==true)};BiwaScheme.isHashtable=function(a){return(a instanceof BiwaScheme.Hashtable)};BiwaScheme.isMutableHashtable=function(a){return(a instanceof BiwaScheme.Hashtable)&&a.mutable};BiwaScheme.isClosure=function(a){return(a instanceof Array)&&(a.closure_p===true)};BiwaScheme.isProcedure=function(a){return BiwaScheme.isClosure(a)||Object.isFunction(a)};BiwaScheme.Parser=Class.create({initialize:function(a){this.tokens=this.tokenize(a);this.i=0},inspect:function(){return["#<Parser:",this.i,"/",this.tokens.length," ",Object.inspect(this.tokens),">"].join("")},tokenize:function(a){var e=new Array(),c=null;var d=0;while(a!=""&&c!=a){c=a;a=a.replace(/^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|[^\s()\[\]{}]+)/,function(g,f){var h=f;if(h=="#|"){d++;return""}else{if(d>0){if(/(.*\|#)/.test(h)){d--;if(d<0){throw new BiwaScheme.Error("Found an extra comment terminator: `|#'")}return h.substring(RegExp.$1.length,h.length)}else{return""}}else{if(h.charAt(0)!=";"){e[e.length]=h}return""}}})}return e},sexpCommentMarker:new Object,getObject:function(){var a=this.getObject0();if(a!=this.sexpCommentMarker){return a}a=this.getObject();if(a==BiwaScheme.Parser.EOS){throw new BiwaScheme.Error("Readable object not found after S exression comment")}a=this.getObject();return a},getList:function(f){var c=BiwaScheme.nil,a=c;while(this.i<this.tokens.length){this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in list)");if(this.tokens[this.i]==")"||this.tokens[this.i]=="]"||this.tokens[this.i]=="}"){this.i++;break}if(this.tokens[this.i]=="."){this.i++;var e=this.getObject();if(e!=BiwaScheme.Parser.EOS&&c!=BiwaScheme.nil){a.cdr=e}}else{var d=new BiwaScheme.Pair(this.getObject(),BiwaScheme.nil);if(c==BiwaScheme.nil){c=d}else{a.cdr=d}a=d}}return c},getVector:function(c){var a=new Array();while(this.i<this.tokens.length){this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in vector)");if(this.tokens[this.i]==")"||this.tokens[this.i]=="]"||this.tokens[this.i]=="}"){this.i++;break}a[a.length]=this.getObject()}return a},eatObjectsInSexpComment:function(a){while(this.tokens[this.i]=="#;"){this.i++;if((this.getObject()==BiwaScheme.Parser.EOS)||(this.i>=this.tokens.length)){throw new BiwaScheme.Error(a)}}},getObject0:function(){if(this.i>=this.tokens.length){return BiwaScheme.Parser.EOS}var a=this.tokens[this.i++];if(a=="#;"){return this.sexpCommentMarker}var c=a=="'"?"quote":a=="`"?"quasiquote":a==","?"unquote":a==",@"?"unquote-splicing":false;if(c||a=="("||a=="#("||a=="["||a=="#["||a=="{"||a=="#{"){return c?new BiwaScheme.Pair(BiwaScheme.Sym(c),new BiwaScheme.Pair(this.getObject(),BiwaScheme.nil)):(a=="("||a=="["||a=="{")?this.getList(a):this.getVector(a)}else{switch(a){case"+inf.0":return Infinity;case"-inf.0":return -Infinity;case"+nan.0":return NaN}var d;if(/^#x[0-9a-z]+$/i.test(a)){d=new Number("0x"+a.substring(2,a.length))}else{if(/^#d[0-9\.]+$/i.test(a)){d=new Number(a.substring(2,a.length))}else{d=new Number(a)}}if(!isNaN(d)){return d.valueOf()}else{if(a=="#f"||a=="#F"){return false}else{if(a=="#t"||a=="#T"){return true}else{if(a.toLowerCase()=="#\\newline"){return BiwaScheme.Char.get("\n")}else{if(a.toLowerCase()=="#\\space"){return BiwaScheme.Char.get(" ")}else{if(a.toLowerCase()=="#\\tab"){return BiwaScheme.Char.get("\t")}else{if(/^#\\.$/.test(a)){return BiwaScheme.Char.get(a.charAt(2))}else{if(/^\"(\\(.|$)|[^\"\\])*\"?$/.test(a)){return a.replace(/(\r?\n|\\n)/g,"\n").replace(/^\"|\\(.|$)|\"$/g,function(f,e){return e?e:""})}else{return BiwaScheme.Sym(a)}}}}}}}}}}});BiwaScheme.Parser.EOS=new Object();BiwaScheme.Compiler=Class.create({initialize:function(){},is_tail:function(a){return(a[0]=="return")},collect_free:function(h,g,d){var f=h;var j=d;var a=f.arr;for(var c=0;c<a.length;c++){j=this.compile_refer(a[c],g,["argument",j])}return j},compile_refer:function(a,d,c){return this.compile_lookup(a,d,function(e){return["refer-local",e,c]},function(e){return["refer-free",e,c]},function(e){return["refer-global",e,c]})},compile_lookup:function(a,g,j,k,d){var f=g[0],h=g[1];if((n=f.index(a))!=null){return j(n)}else{if((n=h.index(a))!=null){return k(n)}else{var c=a.name;return d(c)}}},make_boxes:function(f,g,e){var g=g;var j=0;var c=[];while(g instanceof BiwaScheme.Pair){if(f.member(g.car)){c.push(j)}j++;g=g.cdr}var h=e;for(var d=c.length-1;d>=0;d--){h=["box",c[d],h]}return h},find_sets:function(l,o){var j=null;if(l instanceof BiwaScheme.Symbol){j=new BiwaScheme.Set()}else{if(l instanceof BiwaScheme.Pair){switch(l.first()){case BiwaScheme.Sym("define"):var f=l.third();j=this.find_sets(f,o);case BiwaScheme.Sym("begin"):j=this.find_sets(l.cdr,o);break;case BiwaScheme.Sym("quote"):j=new BiwaScheme.Set();break;case BiwaScheme.Sym("lambda"):var k=l.second(),g=l.cdr.cdr;if(k instanceof BiwaScheme.Pair){j=this.find_sets(g,o.set_minus(k.to_set()))}else{j=this.find_sets(g,o.set_minus(new BiwaScheme.Set(k)))}break;case BiwaScheme.Sym("if"):var q=l.second(),e=l.third(),h=l.fourth();j=this.find_sets(q,o).set_union(this.find_sets(e,o),this.find_sets(h,o));break;case BiwaScheme.Sym("set!"):var c=l.second(),a=l.third();if(o.member(c)){j=this.find_sets(a,o).set_cons(c)}else{j=this.find_sets(a,o)}break;case BiwaScheme.Sym("call/cc"):var f=l.second();j=this.find_sets(f,o);break;default:var m=new BiwaScheme.Set();for(var d=l;d instanceof BiwaScheme.Pair;d=d.cdr){m=m.set_union(this.find_sets(d.car,o))}j=m;break}}else{j=new BiwaScheme.Set()}}if(j==null){throw new BiwaScheme.Bug("find_sets() exited in unusual way")}else{return j}},find_free:function(o,m,h){var k=null;if(o instanceof BiwaScheme.Symbol){if(h.member(o)){k=new BiwaScheme.Set(o)}else{k=new BiwaScheme.Set()}}else{if(o instanceof BiwaScheme.Pair){switch(o.first()){case BiwaScheme.Sym("define"):var e=o.third();k=this.find_free(e,m,h);break;case BiwaScheme.Sym("begin"):k=this.find_free(o.cdr,m,h);break;case BiwaScheme.Sym("quote"):k=new BiwaScheme.Set();break;case BiwaScheme.Sym("lambda"):var l=o.second(),g=o.cdr.cdr;if(l instanceof BiwaScheme.Pair){k=this.find_free(g,m.set_union(l.to_set()),h)}else{k=this.find_free(g,m.set_cons(l),h)}break;case BiwaScheme.Sym("if"):var r=o.second(),d=o.third(),j=o.fourth();k=this.find_free(r,m,h).set_union(this.find_free(d,m,h),this.find_free(j,m,h));break;case BiwaScheme.Sym("set!"):var a=o.second(),e=o.third();if(h.member(a)){k=this.find_free(e,m,h).set_cons(a)}else{k=this.find_free(e,m,h)}break;case BiwaScheme.Sym("call/cc"):var e=o.second();k=this.find_free(e,m,h);break;default:var q=new BiwaScheme.Set();for(var c=o;c instanceof BiwaScheme.Pair;c=c.cdr){q=q.set_union(this.find_free(c.car,m,h))}k=q;break}}else{k=new BiwaScheme.Set()}}if(k==null){throw new BiwaScheme.Bug("find_free() exited in unusual way")}else{return k}},find_dot_pos:function(c){var a=0;for(;c instanceof BiwaScheme.Pair;c=c.cdr,++a){}if(c!=BiwaScheme.nil){return a}else{return -1}},last_pair:function(a){if(a instanceof BiwaScheme.Pair){for(;a.cdr instanceof BiwaScheme.Pair;a=a.cdr){}}return a},dotted2proper:function(a){var e=function(g){var h=BiwaScheme.nil;for(;g instanceof BiwaScheme.Pair;){var j=g.cdr;g.cdr=h;h=g;g=j}return h};var d=function(g){var h=BiwaScheme.nil;for(;g instanceof BiwaScheme.Pair;g=g.cdr){h=new BiwaScheme.Pair(g.car,h)}return e(h)};if(a instanceof BiwaScheme.Pair){var f=this.last_pair(a);if(f instanceof BiwaScheme.Pair&&f.cdr===BiwaScheme.nil){return a}else{var c=d(a);this.last_pair(c).cdr=new BiwaScheme.Pair(f.cdr,BiwaScheme.nil);return c}}else{return new BiwaScheme.Pair(a,BiwaScheme.nil)}},compile:function(t,K,C,J,G){var O=null;while(1){if(t instanceof BiwaScheme.Symbol){return this.compile_refer(t,K,(C.member(t)?["indirect",G]:G))}else{if(t instanceof BiwaScheme.Pair){switch(t.first()){case BiwaScheme.Sym("define"):if(t.length()==1){throw new BiwaScheme.Error("Invalid `define': "+t.to_write())}var j=t.cdr.car;var q=t.cdr.cdr;if(j instanceof BiwaScheme.Symbol){if(q===BiwaScheme.nil){t=BiwaScheme.undef}else{if(q.cdr!==BiwaScheme.nil){throw new BiwaScheme.Error("Invalid `define': "+t.to_write())}t=q.car}BiwaScheme.TopEnv[j.name]=BiwaScheme.undef;G=["assign-global",j.name,G]}else{if(j instanceof BiwaScheme.Pair){var N=j.car,h=j.cdr;var z=new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),new BiwaScheme.Pair(h,q));t=z;BiwaScheme.TopEnv[N.name]=BiwaScheme.undef;G=["assign-global",N.name,G]}else{throw new BiwaScheme.Error("compile: define needs a leftbol or pair: got "+j)}}break;case BiwaScheme.Sym("begin"):var M=[];for(var F=t.cdr;F instanceof BiwaScheme.Pair;F=F.cdr){M.push(F.car)}var L=G;for(var I=M.length-1;I>=0;I--){L=this.compile(M[I],K,C,J,L)}return L;case BiwaScheme.Sym("quote"):if(t.length()<2){throw new BiwaScheme.Error("Invalid quote: "+t.to_write())}var A=t.second();return["constant",A,G];case BiwaScheme.Sym("lambda"):if(t.length()<3){throw new BiwaScheme.Error("Invalid lambda: "+t.to_write())}var D=t.cdr.car;var u=new BiwaScheme.Pair(BiwaScheme.Sym("begin"),t.cdr.cdr);var m=this.find_dot_pos(D);var B=this.dotted2proper(D);var y=this.find_free(u,B.to_set(),J);var d=this.find_sets(u,B.to_set());var H=this.compile(u,[B.to_set(),y],d.set_union(C.set_intersect(y)),J.set_union(B.to_set()),["return"]);var r=["close",y.size(),this.make_boxes(d,B,H),G,m];return this.collect_free(y,K,r);case BiwaScheme.Sym("if"):if(t.length()<3||t.length()>4){throw new BiwaScheme.Error("Invalid if: "+t.to_write())}var E=t.second(),l=t.third(),o=t.fourth();var l=this.compile(l,K,C,J,G);var o=this.compile(o,K,C,J,G);t=E;G=["test",l,o];break;case BiwaScheme.Sym("set!"):if(t.length()!=3){throw new BiwaScheme.Error("Invalid set!: "+t.to_write())}var w=t.second(),t=t.third();var g=this.compile_lookup(w,K,function(a){return["assign-local",a,G]},function(a){return["assign-free",a,G]},function(a){return["assign-global",a,G]});G=g;break;case BiwaScheme.Sym("call/cc"):var t=t.second();var L=["conti",(this.is_tail(G)?(K[0].size()+1):0),["argument",["constant",1,["argument",this.compile(t,K,C,J,(this.is_tail(G)?["shift",1,["apply"]]:["apply"]))]]]];return this.is_tail(G)?L:["frame",L,G];default:var k=t.car;var h=t.cdr;var L=this.compile(k,K,C,J,this.is_tail(G)?["shift",h.length(),["apply"]]:["apply"]);L=this.compile(h.length(),K,C,J,["argument",L]);for(var F=h;F instanceof BiwaScheme.Pair;F=F.cdr){L=this.compile(F.car,K,C,J,["argument",L])}return this.is_tail(G)?L:["frame",L,G]}}else{return["constant",t,G]}}}},run:function(a){return this.compile(a,[new BiwaScheme.Set(),new BiwaScheme.Set()],new BiwaScheme.Set(),new BiwaScheme.Set(),["halt"])}});BiwaScheme.Compiler.compile=function(c,a){c=(new BiwaScheme.Interpreter).expand(c);return(new BiwaScheme.Compiler).run(c,a)};BiwaScheme.Pause=Class.create({initialize:function(a){this.on_pause=a},set_state:function(d,a,g,h,e){this.interpreter=d;this.x=a;this.f=g;this.c=h;this.s=e},ready:function(){this.on_pause(this)},resume:function(a){return this.interpreter.resume(true,a,this.x,this.f,this.c,this.s)}});BiwaScheme.Call=Class.create({initialize:function(a,c,d){this.proc=a;this.args=c;this.after=d||function(e){return e[0]}},inspect:function(){return"#<Call args="+this.args.inspect()+">"},toString:function(){return"#<Call>"},to_write:function(){return"#<Call>"}});BiwaScheme.Iterator={ForArray:Class.create({initialize:function(a){this.arr=a;this.i=0},has_next:function(){return this.i<this.arr.length},next:function(){return this.arr[this.i++]}}),ForString:Class.create({initialize:function(a){this.str=a;this.i=0},has_next:function(){return this.i<this.str.length},next:function(){return BiwaScheme.Char.get(this.str.charAt(this.i++))}}),ForList:Class.create({initialize:function(a){this.ls=a},has_next:function(){return(this.ls instanceof BiwaScheme.Pair)&&this.ls!=BiwaScheme.nil},next:function(){var a=this.ls;this.ls=this.ls.cdr;return a}}),ForMulti:Class.create({initialize:function(a){this.objs=a;this.size=a.length;this.iterators=a.map(function(c){return BiwaScheme.Iterator.of(c)})},has_next:function(){for(var a=0;a<this.size;a++){if(!this.iterators[a].has_next()){return false}}return true},next:function(){return this.iterators.map(function(a){return a.next()})}}),of:function(a){switch(true){case (a instanceof Array):return new this.ForArray(a);case (typeof(a)=="string"):return new this.ForString(a);case (a instanceof BiwaScheme.Pair):case (a===BiwaScheme.nil):return new this.ForList(a);default:throw new BiwaScheme.Bug("Iterator.of: unknown class: "+Object.inspect(a))}}};BiwaScheme.Call.default_callbacks={call:function(a){return new BiwaScheme.Call(this.proc,[a])},result:Prototype.emptyFunction,finish:Prototype.emptyFunction};BiwaScheme.Call.foreach=function(g,f,d){d||(d=false);["call","result","finish"].each(function(h){if(!f[h]){f[h]=BiwaScheme.Call.default_callbacks[h]}});var e=null;var a=null;var c=function(j){if(e){var k=f.result(j[0],a);if(k!==undefined){return k}}else{if(d){e=new BiwaScheme.Iterator.ForMulti(g)}else{e=BiwaScheme.Iterator.of(g)}}if(!e.has_next()){return f.finish()}else{a=e.next();var h=f.call(a);h.after=c;return h}};return c(null)};BiwaScheme.Call.multi_foreach=function(c,a){return BiwaScheme.Call.foreach(c,a,true)};BiwaScheme.Interpreter=Class.create({initialize:function(a){this.stack=[];this.on_error=a||function(c){};this.after_evaluate=Prototype.emptyFunction},inspect:function(){return["#<Interpreter: stack size=>",this.stack.length," ","after_evaluate=",Object.inspect(this.after_evaluate),">"].join("")},push:function(a,c){this.stack[c]=a;return c+1},save_stack:function(d){var a=[];for(var c=0;c<d;c++){a[c]=this.stack[c]}return a},restore_stack:function(a){var d=a.length;for(var c=0;c<d;c++){this.stack[c]=a[c]}return d},continuation:function(c,d){var a=this.push(d,c);return this.closure(["refer-local",0,["nuate",this.save_stack(a),["return"]]],0,null,-1)},shift_args:function(e,a,d){for(var c=e-1;c>=-1;c--){this.index_set(d,c+a+1,this.index(d,c))}return d-a-1},index:function(c,a){return this.stack[c-a-2]},index_set:function(d,c,a){this.stack[d-c-2]=a},closure:function(a,g,e,f){var c=[];c[0]=a;for(var d=0;d<g;d++){c[d+1]=this.index(e,d-1)}c[g+1]=f;c.closure_p=true;return c},execute:function(g,d,l,o,j){var h=null;try{h=this._execute(g,d,l,o,j)}catch(m){var k={a:g,x:d,f:l,c:o,s:j,stack:this.stack};return this.on_error(m,k)}return h},run_dump_hook:function(e,d,k,l,h){var g;var j;if(this.dumper){g=this.dumper}else{if(BiwaScheme.Interpreter.dumper){g=BiwaScheme.Interpreter.dumper}else{return}}if(g){var j=new Hash({a:e,f:k,c:l,s:h,x:d,stack:this.stack});g.dump(j)}},_execute:function(I,t,E,H,A){var J=null;while(true){this.run_dump_hook(I,t,E,H,A);switch(t[0]){case"halt":return I;case"refer-local":var B=t[1],t=t[2];I=this.index(E,B);break;case"refer-free":var B=t[1],t=t[2];I=H[B+1];break;case"refer-global":var y=t[1],t=t[2];if(BiwaScheme.TopEnv.hasOwnProperty(y)){var K=BiwaScheme.TopEnv[y]}else{if(BiwaScheme.CoreEnv.hasOwnProperty(y)){var K=BiwaScheme.CoreEnv[y]}else{throw new BiwaScheme.Error("execute: unbound symbol: "+Object.inspect(y))}}I=K;break;case"indirect":var t=t[1];I=I[0];break;case"constant":var z=t[1],t=t[2];I=z;break;case"close":var g=t;var B=g[1],w=g[2],t=g[3],k=g[4];I=this.closure(w,B,A,k);A-=B;break;case"box":var B=t[1],t=t[2];this.index_set(A,B,[this.index(A,B)]);break;case"test":var l=t[1],m=t[2];t=((I!==false)?l:m);break;case"assign-global":var L=t[1],t=t[2];if(!BiwaScheme.TopEnv.hasOwnProperty(L)&&!BiwaScheme.CoreEnv.hasOwnProperty(L)){throw new BiwaScheme.Error("global variable '"+L+"' is not defined")}BiwaScheme.TopEnv[L]=I;I=BiwaScheme.undef;break;case"assign-local":var B=t[1],t=t[2];var u=this.index(E,B);u[0]=I;I=BiwaScheme.undef;break;case"assign-free":var B=t[1],t=t[2];var u=H[B+1];u[0]=I;I=BiwaScheme.undef;break;case"conti":var B=t[1],t=t[2];I=this.continuation(A,B);break;case"nuate":var q=t[1],t=t[2];A=this.restore_stack(q);break;case"frame":var J=t[2];t=t[1];A=this.push(J,this.push(E,this.push(H,A)));break;case"argument":var t=t[1];A=this.push(I,A);break;case"shift":var B=t[1],t=t[2];var e=this.index(A,B);A=this.shift_args(B,e,A);break;case"apply":var h=I;var e=this.index(A,-1);if(h instanceof Array){I=h;t=h[0];var k=h[h.length-1];if(k>=0){var o=BiwaScheme.nil;for(var D=e;--D>=k;){o=new BiwaScheme.Pair(this.index(A,D),o)}if(k>=e){for(var D=-1;D<e;D++){this.index_set(A,D-1,this.index(A,D))}A++;this.index_set(A,-1,this.index(A,-1)+1)}this.index_set(A,k,o)}E=A;H=I}else{if(h instanceof Function){var d=[];for(var D=0;D<e;D++){d.push(this.index(A,D))}var v=h(d,this);if(v instanceof BiwaScheme.Pause){var j=v;j.set_state(this,["return"],E,H,A);j.ready();return j}else{if(v instanceof BiwaScheme.Call){var G=["frame",["argument",["constant",1,["argument",["constant",v.after,["apply"]]]]],["return"]];var r=["constant",v.args.length,["argument",["constant",v.proc,["apply",v.args.length]]]];var F=v.args.inject(r,function(c,a){return["constant",a,["argument",c]]});t=["frame",F,G]}else{I=v;t=["return"]}}}else{throw new BiwaScheme.Error(Object.inspect(h)+" is not a function")}}break;case"return":var B=this.index(A,-1);var C=A-B;t=this.index(C,0),E=this.index(C,1),H=this.index(C,2),A=C-3-1;break;default:throw new BiwaScheme.Bug("unknown opecode type: "+t[0])}}return I},expand:function(o,k){k||(k={});var h=null;if(o instanceof BiwaScheme.Symbol){h=o}else{if(o instanceof BiwaScheme.Pair){switch(o.car){case BiwaScheme.Sym("define"):var d=o.cdr.car,e=o.cdr.cdr;h=new BiwaScheme.Pair(BiwaScheme.Sym("define"),new BiwaScheme.Pair(d,this.expand(e,k)));break;case BiwaScheme.Sym("begin"):h=new BiwaScheme.Pair(BiwaScheme.Sym("begin"),this.expand(o.cdr,k));break;case BiwaScheme.Sym("quote"):h=o;break;case BiwaScheme.Sym("lambda"):var j=o.cdr.car,f=o.cdr.cdr;h=new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),new BiwaScheme.Pair(j,this.expand(f,k)));break;case BiwaScheme.Sym("if"):var s=o.second(),c=o.third(),g=o.fourth();if(g==BiwaScheme.inner_of_nil){g=BiwaScheme.undef}h=[BiwaScheme.Sym("if"),this.expand(s,k),this.expand(c,k),this.expand(g,k)].to_list();break;case BiwaScheme.Sym("set!"):var r=o.second(),o=o.third();h=[BiwaScheme.Sym("set!"),r,this.expand(o,k)].to_list();break;case BiwaScheme.Sym("call-with-current-continuation"):case BiwaScheme.Sym("call/cc"):var o=o.second();h=[BiwaScheme.Sym("call/cc"),this.expand(o,k)].to_list();break;default:if(o.car instanceof BiwaScheme.Symbol&&BiwaScheme.TopEnv[o.car.name] instanceof BiwaScheme.Syntax){var l=BiwaScheme.TopEnv[o.car.name];k.modified=true;h=l.transform(o);if(BiwaScheme.Debug){var m=BiwaScheme.to_write(o);var a=BiwaScheme.to_write(h);if(m!=a){puts("expand: "+m+" => "+a)}}var q;for(;;){h=this.expand(h,q={});if(!q.modified){break}}}else{if(o==BiwaScheme.nil){h=BiwaScheme.nil}else{h=new BiwaScheme.Pair(this.expand(o.car,k),o.cdr.to_array().map(function(t){return this.expand(t,k)}.bind(this)).to_list())}}}}else{h=o}}return h},evaluate:function(c,a){this.parser=new BiwaScheme.Parser(c);this.compiler=new BiwaScheme.Compiler();if(a){this.after_evaluate=a}if(BiwaScheme.Debug){puts("executing: "+c)}this.is_top=true;this.file_stack=[];return this.resume(false)},resume:function(e,j,k,d,h,o){var g=BiwaScheme.undef;for(;;){if(e){g=this.execute(j,k,d,h,o);e=false}else{if(!this.parser){break}var l=this.parser.getObject();if(l===BiwaScheme.Parser.EOS){break}l=this.expand(l);var m=this.compiler.run(l);g=this.execute(l,m,0,[],0)}if(g instanceof BiwaScheme.Pause){return g}}this.after_evaluate(g);return g},invoke_closure:function(f,d){d||(d=[]);var c=d.length;var a=["constant",c,["argument",["constant",f,["apply"]]]];for(var e=0;e<c;e++){a=["constant",d[e],["argument",a]]}return this.execute(f,["frame",a,["halt"]],0,f,0)},compile:function(c){var a=BiwaScheme.Interpreter.read(c);var d=BiwaScheme.Compiler.compile(a);return d}});BiwaScheme.Interpreter.read=function(c){var d=new BiwaScheme.Parser(c);var a=d.getObject();return(a==BiwaScheme.Parser.EOS)?BiwaScheme.eof:a};BiwaScheme.check_arity=function(c,d,a){var e=arguments.callee.caller?arguments.callee.caller.fname:"(?)";if(c<d){if(a&&a==d){throw new BiwaScheme.Error(e+": wrong number of arguments (expected: "+d+" got: "+c+")")}else{throw new BiwaScheme.Error(e+": too few arguments (at least: "+d+" got: "+c+")")}}else{if(a&&a<c){throw new BiwaScheme.Error(e+": too many arguments (at most: "+a+" got: "+c+")")}}};BiwaScheme.define_libfunc=function(h,c,a,d,g){var e=function(k,j){BiwaScheme.check_arity(k.length,c,a);var f=d(k,j);if(g){return f}else{if(f===undefined){throw new BiwaScheme.Bug("library function `"+h+"' returned JavaScript's undefined")}else{if(f===null){throw new BiwaScheme.Bug("library function `"+h+"' returned JavaScript's null")}else{return f}}}};d.fname=h;e.fname=h;e.inspect=function(){return this.fname};BiwaScheme.CoreEnv[h]=e};BiwaScheme.define_libfunc_raw=function(e,c,a,d){BiwaScheme.define_libfunc(e,c,a,d,true)};BiwaScheme.define_syntax=function(a,d){var c=new BiwaScheme.Syntax(a,d);BiwaScheme.TopEnv[a]=c};BiwaScheme.define_scmfunc=function(e,c,a,d){(new Interpreter).evaluate("(define "+e+" "+d+"\n)")};var make_assert=function(a){return function(){var c=arguments.callee.caller?arguments.callee.caller.fname:"";a.apply(this,[c].concat($A(arguments)))}};var make_simple_assert=function(a,c){return make_assert(function(f,e,d){option=d?("("+d+")"):"";if(!c(e)){throw new BiwaScheme.Error(f+option+": "+a+" required, but got "+BiwaScheme.to_write(e))}})};var assert_number=make_simple_assert("number",function(a){return typeof(a)=="number"||(a instanceof BiwaScheme.Complex)});var assert_integer=make_simple_assert("integer",function(a){return typeof(a)=="number"&&(a%1==0)});var assert_real=make_simple_assert("real number",function(a){return typeof(a)=="number"});var assert_between=make_assert(function(e,a,d,c){if(typeof(a)!="number"||a!=Math.round(a)){throw new BiwaScheme.Error(e+": number required, but got "+BiwaScheme.to_write(a))}if(a<d||c<a){throw new BiwaScheme.Error(e+": number must be between "+d+" and "+c+", but got "+BiwaScheme.to_write(a))}});var assert_string=make_simple_assert("string",Object.isString);var assert_char=make_simple_assert("character",BiwaScheme.isChar);var assert_symbol=make_simple_assert("symbol",BiwaScheme.isSymbol);var assert_port=make_simple_assert("port",BiwaScheme.isPort);var assert_pair=make_simple_assert("pair",BiwaScheme.isPair);var assert_list=make_simple_assert("list",BiwaScheme.isList);var assert_vector=make_simple_assert("vector",BiwaScheme.isVector);var assert_hashtable=make_simple_assert("hashtable",BiwaScheme.isHashtable);var assert_mutable_hashtable=make_simple_assert("mutable hashtable",BiwaScheme.isMutableHashtable);var assert_record=make_simple_assert("record",BiwaScheme.isRecord);var assert_record_td=make_simple_assert("record type descriptor",BiwaScheme.isRecordTD);var assert_record_cd=make_simple_assert("record constructor descriptor",BiwaScheme.isRecordCD);var assert_function=make_simple_assert("JavaScript function",Object.isFunction);var assert_closure=make_simple_assert("scheme function",BiwaScheme.isClosure);var assert_procedure=make_simple_assert("scheme/js function",function(a){return BiwaScheme.isClosure(a)||Object.isFunction(a)});var assert_date=make_simple_assert("date",function(a){return a instanceof Date});var assert=make_assert(function(d,c,a){if(!c){throw new BiwaScheme.Error(d+": "+a)}});if(typeof(BiwaScheme)!="object"){BiwaScheme={}}with(BiwaScheme){define_syntax("cond",function(a){var d=a.cdr;if(!(d instanceof Pair)||d===nil){throw new Error("malformed cond: cond needs list but got "+to_write_ss(d))}var c=null;d.to_array().reverse().each(function(g){if(!(g instanceof Pair)){throw new Error("bad clause in cond: "+to_write_ss(g))}if(g.car===Sym("else")){if(c!==null){throw new Error("'else' clause of cond followed by more clauses: "+to_write_ss(d))}else{if(g.cdr===nil){c=false}else{if(g.cdr.cdr===nil){c=g.cdr.car}else{c=new Pair(Sym("begin"),g.cdr)}}}}else{if(c===null){c=BiwaScheme.undef}else{var h=g.car;if(g.cdr===nil){c=[Sym("or"),h,c].to_list()}else{if(g.cdr.cdr===nil){c=[Sym("if"),h,g.cdr.car,c].to_list()}else{if(g.cdr.car===Sym("=>")){var h=g.car,f=g.cdr.cdr.car;var e=BiwaScheme.gensym();c=List(Sym("let"),List(List(e,h)),List(Sym("if"),h,List(f,e),c))}else{c=[Sym("if"),h,new Pair(Sym("begin"),g.cdr),c].to_list()}}}}}});return c});define_syntax("case",function(a){var e=BiwaScheme.gensym();if(a.cdr===nil){throw new Error("case: at least one clause is required")}else{if(!(a.cdr instanceof Pair)){throw new Error("case: proper list is required")}else{var d=a.cdr.car;var f=a.cdr.cdr;var c=undefined;f.to_array().reverse().each(function(g){if(g.car===Sym("else")){if(c===undefined){c=new Pair(Sym("begin"),g.cdr)}else{throw new Error("case: 'else' clause followed by more clauses: "+to_write_ss(f))}}else{c=[Sym("if"),new Pair(Sym("or"),g.car.to_array().map(function(h){return[Sym("eqv?"),e,[Sym("quote"),h].to_list()].to_list()}).to_list()),new Pair(Sym("begin"),g.cdr),c].to_list()}});return new Pair(Sym("let1"),new Pair(e,new Pair(d,new Pair(c,nil))))}}});define_syntax("and",function(a){if(a.cdr==nil){return true}var e=a.cdr.to_array();var d=e.length-1;var c=e[d];for(d=d-1;d>=0;d--){c=[Sym("if"),e[d],c,false].to_list()}return c});define_syntax("or",function(a){var e=a.cdr.to_array();var d=false;for(var c=e.length-1;c>=0;c--){d=[Sym("if"),e[c],e[c],d].to_list()}return d});define_syntax("let",function(k){var a=null;if(k.cdr.car instanceof Symbol){a=k.cdr.car;k=k.cdr}var d=k.cdr.car,e=k.cdr.cdr;if(!(d instanceof Pair)){throw new Error("let: need a pair for bindings: got "+to_write(d))}var h=nil,j=nil;for(var c=d;c instanceof Pair;c=c.cdr){h=new Pair(c.car.car,h);j=new Pair(c.car.cdr.car,j)}var g=null;if(a){h=h.to_array().reverse().to_list();j=j.to_array().reverse().to_list();var f=new Pair(Sym("lambda"),new Pair(h,e));var l=new Pair(a,j);g=[Sym("letrec"),new Pair([a,f].to_list(),nil),l].to_list()}else{g=new Pair(new Pair(Sym("lambda"),new Pair(h,e)),j)}return g});define_syntax("let*",function(c){var e=c.cdr.car,a=c.cdr.cdr;if(!(e instanceof Pair)){throw new Error("let*: need a pair for bindings: got "+to_write(e))}var d=null;e.to_array().reverse().each(function(f){d=new Pair(Sym("let"),new Pair(new Pair(f,nil),d==null?a:new Pair(d,nil)))});return d});var expand_letrec_star=function(c){var f=c.cdr.car,a=c.cdr.cdr;if(!(f instanceof Pair)){throw new Error("letrec*: need a pair for bindings: got "+to_write(f))}var e=a;f.to_array().reverse().each(function(g){e=new Pair(new Pair(Sym("set!"),g),e)});var d=nil;f.to_array().reverse().each(function(g){d=new Pair(new Pair(g.car,new Pair(BiwaScheme.undef,nil)),d)});return new Pair(Sym("let"),new Pair(d,e))};define_syntax("letrec",expand_letrec_star);define_syntax("letrec*",expand_letrec_star);define_syntax("let-values",function(c){var g=c.cdr.car;var a=c.cdr.cdr;var d=null;var f=nil;var e=nil;g.to_array().reverse().each(function(k){var o=k.cdr.car;var l=BiwaScheme.gensym();var m=new Pair(l,new Pair(new Pair(Sym("lambda"),new Pair(nil,new Pair(o,nil))),nil));f=new Pair(m,f);var j=k.car;e=new Pair(new Pair(j,new Pair(new Pair(l,nil),nil)),e)});var h=new Pair(Sym("let*-values"),new Pair(e,a));d=new Pair(Sym("let"),new Pair(f,new Pair(h,nil)));return d});define_syntax("let*-values",function(c){var e=c.cdr.car;var a=c.cdr.cdr;var d=null;e.to_array().reverse().each(function(g){var f=g.car,h=g.cdr.car;d=new Pair(Sym("call-with-values"),new Pair(new Pair(Sym("lambda"),new Pair(nil,new Pair(h,nil))),new Pair(new Pair(Sym("lambda"),new Pair(f,(d==null?a:new Pair(d,nil)))),nil)))});return d});BiwaScheme.eq=function(d,c){return d===c};BiwaScheme.eqv=function(d,c){return d==c&&(typeof(d)==typeof(c))};define_libfunc("eqv?",2,2,function(a){return BiwaScheme.eqv(a[0],a[1])});define_libfunc("eq?",2,2,function(a){return BiwaScheme.eq(a[0],a[1])});define_libfunc("equal?",2,2,function(a){return to_write(a[0])==to_write(a[1])});define_libfunc("procedure?",1,1,function(a){return((a[0] instanceof Array)&&(a[0].closure_p===true)||(typeof a[0]=="function"))});define_libfunc("number?",1,1,function(a){return(typeof(a[0])=="number")||(a[0] instanceof Complex)||(a[0] instanceof Rational)});define_libfunc("complex?",1,1,function(a){return(a[0] instanceof Complex)});define_libfunc("real?",1,1,function(a){return(typeof(a[0])=="number")});define_libfunc("rational?",1,1,function(a){return(a[0] instanceof Rational)});define_libfunc("integer?",1,1,function(a){return typeof(a[0])=="number"&&a[0]==Math.round(a[0])&&a[0]!=Infinity&&a[0]!=-Infinity});define_libfunc("=",2,null,function(c){var a=c[0];assert_number(c[0]);for(var d=1;d<c.length;d++){assert_number(c[d]);if(c[d]!=a){return false}}return true});define_libfunc("<",2,null,function(a){assert_number(a[0]);for(var c=1;c<a.length;c++){assert_number(a[c]);if(!(a[c-1]<a[c])){return false}}return true});define_libfunc(">",2,null,function(a){assert_number(a[0]);for(var c=1;c<a.length;c++){assert_number(a[c]);if(!(a[c-1]>a[c])){return false}}return true});define_libfunc("<=",2,null,function(a){assert_number(a[0]);for(var c=1;c<a.length;c++){assert_number(a[c]);if(!(a[c-1]<=a[c])){return false}}return true});define_libfunc(">=",2,null,function(a){assert_number(a[0]);for(var c=1;c<a.length;c++){assert_number(a[c]);if(!(a[c-1]>=a[c])){return false}}return true});define_libfunc("zero?",1,1,function(a){assert_number(a[0]);return a[0]===0});define_libfunc("positive?",1,1,function(a){assert_number(a[0]);return(a[0]>0)});define_libfunc("negative?",1,1,function(a){assert_number(a[0]);return(a[0]<0)});define_libfunc("odd?",1,1,function(a){assert_number(a[0]);return(a[0]%2==1)||(a[0]%2==-1)});define_libfunc("even?",1,1,function(a){assert_number(a[0]);return a[0]%2==0});define_libfunc("finite?",1,1,function(a){assert_number(a[0]);return(a[0]!=Infinity)&&(a[0]!=-Infinity)&&!isNaN(a[0])});define_libfunc("infinite?",1,1,function(a){assert_number(a[0]);return(a[0]==Infinity)||(a[0]==-Infinity)});define_libfunc("nan?",1,1,function(a){assert_number(a[0]);return isNaN(a[0])});define_libfunc("max",2,null,function(a){for(var c=0;c<a.length;c++){assert_number(a[c])}return Math.max.apply(null,a)});define_libfunc("min",2,null,function(a){for(var c=0;c<a.length;c++){assert_number(a[c])}return Math.min.apply(null,a)});define_libfunc("+",0,null,function(a){var d=0;for(var c=0;c<a.length;c++){assert_number(a[c]);d+=a[c]}return d});define_libfunc("*",0,null,function(a){var d=1;for(var c=0;c<a.length;c++){assert_number(a[c]);d*=a[c]}return d});define_libfunc("-",1,null,function(c){var a=c.length;assert_number(c[0]);if(a==1){return -c[0]}else{var e=c[0];for(var d=1;d<a;d++){assert_number(c[d]);e-=c[d]}return e}});define_libfunc("/",1,null,function(c){var a=c.length;assert_number(c[0]);if(a==1){return 1/c[0]}else{var e=c[0];for(var d=1;d<a;d++){assert_number(c[d]);e/=c[d]}return e}});define_libfunc("abs",1,1,function(a){assert_number(a[0]);return Math.abs(a[0])});var div=function(c,a){return Math.floor(c/a)};var mod=function(c,a){return c-Math.floor(c/a)*a};var div0=function(c,a){return(c>0)?Math.floor(c/a):Math.ceil(c/a)};var mod0=function(c,a){return(c>0)?c-Math.floor(c/a)*a:c-Math.ceil(c/a)*a};define_libfunc("div0-and-mod0",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return new Values([div(a[0],a[1]),mod(a[0],a[1])])});define_libfunc("div",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return div(a[0],a[1])});define_libfunc("mod",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return mod(a[0],a[1])});define_libfunc("div0-and-mod0",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return new Values([div0(a[0],a[1]),mod0(a[0],a[1])])});define_libfunc("div0",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return div0(a[0],a[1])});define_libfunc("mod0",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return mod0(a[0],a[1])});define_libfunc("numerator",1,1,function(a){assert_number(a[0]);if(a[0] instanceof Rational){return a[0].numerator}else{throw new Bug("todo")}});define_libfunc("denominator",1,1,function(a){assert_number(a[0]);if(a[0] instanceof Rational){return a[0].denominator}else{throw new Bug("todo")}});define_libfunc("floor",1,1,function(a){assert_number(a[0]);return Math.floor(a[0])});define_libfunc("ceiling",1,1,function(a){assert_number(a[0]);return Math.ceil(a[0])});define_libfunc("truncate",1,1,function(a){assert_number(a[0]);return(a[0]<0)?Math.ceil(a[0]):Math.floor(a[0])});define_libfunc("round",1,1,function(a){assert_number(a[0]);return Math.round(a[0])});define_libfunc("exp",1,1,function(a){assert_number(a[0]);return Math.exp(a[0])});define_libfunc("log",1,2,function(a){var c=a[0],d=a[1];assert_number(c);if(d){assert_number(d);return Math.log(c)/Math.log(b)}else{return Math.log(c)}});define_libfunc("sin",1,1,function(a){assert_number(a[0]);return Math.sin(a[0])});define_libfunc("cos",1,1,function(a){assert_number(a[0]);return Math.cos(a[0])});define_libfunc("tan",1,1,function(a){assert_number(a[0]);return Math.tan(a[0])});define_libfunc("asin",1,1,function(a){assert_number(a[0]);return Math.asin(a[0])});define_libfunc("acos",1,1,function(a){assert_number(a[0]);return Math.acos(a[0])});define_libfunc("atan",1,2,function(a){assert_number(a[0]);if(a[1]){assert_number(a[1]);return Math.atan2(a[0],a[1])}else{return Math.atan(a[0])}});define_libfunc("sqrt",1,1,function(a){assert_number(a[0]);return Math.sqrt(a[0])});define_libfunc("exact-integer-sqrt",1,1,function(c){assert_number(c[0]);var a=Math.sqrt(c[0]);var e=a-(a%1);var d=c[0]-e*e;return new Values([e,d])});define_libfunc("expt",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return Math.pow(a[0],a[1])});define_libfunc("make-rectangular",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return new Complex(a[0],a[1])});define_libfunc("make-polar",2,2,function(a){assert_number(a[0]);assert_number(a[1]);return Complex.from_polar(a[0],a[1])});define_libfunc("real-part",1,1,function(a){assert_number(a[0]);return Complex.assure(a[0]).real});define_libfunc("imag-part",1,1,function(a){assert_number(a[0]);return Complex.assure(a[0]).imag});define_libfunc("magnitude",1,1,function(a){assert_number(a[0]);return Complex.assure(a[0]).magnitude()});define_libfunc("angle",1,1,function(a){assert_number(a[0]);return Complex.assure(a[0]).angle()});define_libfunc("number->string",1,3,function(c){var e=c[0],d=c[1],a=c[2];if(a){throw new Bug("number->string: precision is not yet implemented")}d=d||10;return e.toString(d)});define_libfunc("string->number",1,3,function(a){var d=a[0],c=a[1]||10;switch(d){case"+inf.0":return Infinity;case"-inf.0":return -Infinity;case"+nan.0":return NaN;default:if(/[eE.]/.match(d)){return parseFloat(d)}else{return parseInt(d,c)}}});define_libfunc("not",1,1,function(a){return(a[0]===false)?true:false});define_libfunc("boolean?",1,1,function(a){return(a[0]===false||a[0]===true)?true:false});define_libfunc("boolean=?",2,null,function(c){var a=c.length;for(var d=1;d<a;d++){if(c[d]!=c[0]){return false}}return true});define_libfunc("pair?",1,1,function(a){return(a[0] instanceof Pair)?true:false});define_libfunc("cons",2,2,function(a){return new Pair(a[0],a[1])});define_libfunc("car",1,1,function(a){if(!(a[0] instanceof Pair)){throw new Error("Attempt to apply car on "+a[0])}return a[0].car});define_libfunc("cdr",1,1,function(a){if(!(a[0] instanceof Pair)){throw new Error("Attempt to apply cdr on "+a[0])}return a[0].cdr});define_libfunc("set-car!",2,2,function(a){if(!(a[0] instanceof Pair)){throw new Error("Attempt to apply set-car! on "+a[0])}a[0].car=a[1];return BiwaScheme.undef});define_libfunc("set-cdr!",2,2,function(a){if(!(a[0] instanceof Pair)){throw new Error("Attempt to apply set-cdr! on "+a[0])}a[0].cdr=a[1];return BiwaScheme.undef});define_libfunc("caar",1,1,function(a){return a[0].car.car});define_libfunc("cadr",1,1,function(a){return a[0].cdr.car});define_libfunc("cdar",1,1,function(a){return a[0].car.cdr});define_libfunc("cddr",1,1,function(a){return a[0].cdr.cdr});define_libfunc("caaar",1,1,function(a){return a[0].car.car.car});define_libfunc("caadr",1,1,function(a){return a[0].cdr.car.car});define_libfunc("cadar",1,1,function(a){return a[0].car.cdr.car});define_libfunc("caddr",1,1,function(a){return a[0].cdr.cdr.car});define_libfunc("cdaar",1,1,function(a){return a[0].car.car.cdr});define_libfunc("cdadr",1,1,function(a){return a[0].cdr.car.cdr});define_libfunc("cddar",1,1,function(a){return a[0].car.cdr.cdr});define_libfunc("cdddr",1,1,function(a){return a[0].cdr.cdr.cdr});define_libfunc("caaaar",1,1,function(a){return a[0].car.car.car.car});define_libfunc("caaadr",1,1,function(a){return a[0].cdr.car.car.car});define_libfunc("caadar",1,1,function(a){return a[0].car.cdr.car.car});define_libfunc("caaddr",1,1,function(a){return a[0].cdr.cdr.car.car});define_libfunc("cadaar",1,1,function(a){return a[0].car.car.cdr.car});define_libfunc("cadadr",1,1,function(a){return a[0].cdr.car.cdr.car});define_libfunc("caddar",1,1,function(a){return a[0].car.cdr.cdr.car});define_libfunc("cadddr",1,1,function(a){return a[0].cdr.cdr.cdr.car});define_libfunc("cdaaar",1,1,function(a){return a[0].car.car.car.cdr});define_libfunc("cdaadr",1,1,function(a){return a[0].cdr.car.car.cdr});define_libfunc("cdadar",1,1,function(a){return a[0].car.cdr.car.cdr});define_libfunc("cdaddr",1,1,function(a){return a[0].cdr.cdr.car.cdr});define_libfunc("cddaar",1,1,function(a){return a[0].car.car.cdr.cdr});define_libfunc("cddadr",1,1,function(a){return a[0].cdr.car.cdr.cdr});define_libfunc("cdddar",1,1,function(a){return a[0].car.cdr.cdr.cdr});define_libfunc("cddddr",1,1,function(a){return a[0].cdr.cdr.cdr.cdr});define_libfunc("null?",1,1,function(a){return(a[0]===nil)});define_libfunc("list?",1,1,function(a){var c=[];for(var d=a[0];d!=nil;d=d.cdr){if(d==nil){return true}if(!(d instanceof Pair)){return false}if(c.find(function(e){return e===d.car})){return false}c.push(d.car)}return true});define_libfunc("list",0,null,function(c){var a=nil;for(var d=c.length-1;d>=0;d--){a=new Pair(c[d],a)}return a});define_libfunc("length",1,1,function(a){assert_list(a[0]);var d=0;for(var c=a[0];c!=nil;c=c.cdr){d++}return d});define_libfunc("append",2,null,function(c){var a=c.length;var d=c[--a];while(a--){c[a].to_array().reverse().each(function(e){d=new Pair(e,d)})}return d});define_libfunc("reverse",1,1,function(c){if(!c[0] instanceof Pair){throw new Error("reverse needs pair but got "+c[0])}var a=nil;for(var d=c[0];d!=nil;d=d.cdr){a=new Pair(d.car,a)}return a});define_libfunc("list-tail",2,2,function(a){if(!a[0] instanceof Pair){throw new Error("list-tail needs pair but got "+a[0])}var d=a[0];for(var c=0;c<a[1];c++){if(!d instanceof Pair){throw new Error("list-tail: the list is shorter than "+a[1])}d=d.cdr}return d});define_libfunc("list-ref",2,2,function(a){if(!a[0] instanceof Pair){throw new Error("list-ref needs pair but got "+a[0])}var d=a[0];for(var c=0;c<a[1];c++){if(!d instanceof Pair){throw new Error("list-ref: the list is shorter than "+a[1])}d=d.cdr}return d.car});define_libfunc("map",2,null,function(f){var e=f.shift(),c=f;c.each(function(a){assert_list(a)});var d=[];return Call.multi_foreach(c,{call:function(a){return new Call(e,a.map(function(g){return g.car}))},result:function(a){d.push(a)},finish:function(){return d.to_list()}})});define_libfunc("for-each",2,null,function(d){var c=d.shift(),a=d;a.each(function(e){assert_list(e)});return Call.multi_foreach(a,{call:function(e){return new Call(c,e.map(function(f){return f.car}))},finish:function(){return BiwaScheme.undef}})});define_libfunc("symbol?",1,1,function(a){return(a[0] instanceof Symbol)?true:false});define_libfunc("symbol->string",1,1,function(a){assert_symbol(a[0]);return a[0].name});define_libfunc("symbol=?",2,null,function(a){assert_symbol(a[0]);for(var c=1;c<a.length;c++){assert_symbol(a[c]);if(a[c]!=a[0]){return false}}return true});define_libfunc("string->symbol",1,1,function(a){assert_string(a[0]);return Sym(a[0])});define_libfunc("char?",1,1,function(a){return(a[0] instanceof Char)});define_libfunc("char->integer",1,1,function(a){assert_char(a[0]);return a[0].value.charCodeAt(0)});define_libfunc("integer->char",1,1,function(a){assert_integer(a[0]);return Char.get(String.fromCharCode(a[0]))});var make_char_compare_func=function(a){return function(c){assert_char(c[0]);for(var d=1;d<c.length;d++){assert_char(c[d]);if(!a(c[d-1].value,c[d].value)){return false}}return true}};define_libfunc("char=?",2,null,make_char_compare_func(function(d,c){return d==c}));define_libfunc("char<?",2,null,make_char_compare_func(function(d,c){return d<c}));define_libfunc("char>?",2,null,make_char_compare_func(function(d,c){return d>c}));define_libfunc("char<=?",2,null,make_char_compare_func(function(d,c){return d<=c}));define_libfunc("char>=?",2,null,make_char_compare_func(function(d,c){return d>=c}));define_libfunc("string?",1,1,function(a){return(typeof(a[0])=="string")});define_libfunc("make-string",1,2,function(a){assert_integer(a[0]);var d=" ";if(a[1]){assert_char(a[1]);d=a[1].value}return d.times(a[0])});define_libfunc("string",1,null,function(a){for(var c=0;c<a.length;c++){assert_char(a[c])}return a.map(function(d){return d.value}).join("")});define_libfunc("string-length",1,1,function(a){assert_string(a[0]);return a[0].length});define_libfunc("string-ref",2,2,function(a){assert_string(a[0]);assert_between(a[1],0,a[0].length-1);return Char.get(a[0].charAt([a[1]]))});define_libfunc("string=?",2,null,function(a){assert_string(a[0]);for(var c=1;c<a.length;c++){assert_string(a[c]);if(a[0]!=a[c]){return false}}return true});define_libfunc("string<?",2,null,function(a){assert_string(a[0]);for(var c=1;c<a.length;c++){assert_string(a[c]);if(!(a[c-1]<a[c])){return false}}return true});define_libfunc("string>?",2,null,function(a){assert_string(a[0]);for(var c=1;c<a.length;c++){assert_string(a[c]);if(!(a[c-1]>a[c])){return false}}return true});define_libfunc("string<=?",2,null,function(a){assert_string(a[0]);for(var c=1;c<a.length;c++){assert_string(a[c]);if(!(a[c-1]<=a[c])){return false}}return true});define_libfunc("string>=?",2,null,function(a){assert_string(a[0]);for(var c=1;c<a.length;c++){assert_string(a[c]);if(!(a[c-1]>=a[c])){return false}}return true});define_libfunc("substring",3,3,function(a){assert_string(a[0]);assert_integer(a[1]);assert_integer(a[2]);if(a[1]<0){throw new Error("substring: start too small: "+a[1])}if(a[2]<0){throw new Error("substring: end too small: "+a[2])}if(a[0].length+1<=a[1]){throw new Error("substring: start too big: "+a[1])}if(a[0].length+1<=a[2]){throw new Error("substring: end too big: "+a[2])}if(!(a[1]<=a[2])){throw new Error("substring: not start <= end: "+a[1]+", "+a[2])}return a[0].substring(a[1],a[2])});define_libfunc("string-append",0,null,function(a){for(var c=0;c<a.length;c++){assert_string(a[c])}return a.join("")});define_libfunc("string->list",1,1,function(a){assert_string(a[0]);var c=[];a[0].scan(/./,function(d){c.push(Char.get(d[0]))});return c.to_list()});define_libfunc("list->string",1,1,function(a){assert_list(a[0]);return a[0].to_array().map(function(d){return d.value}).join("")});define_libfunc("string-for-each",2,null,function(c){var a=c.shift(),d=c;d.each(function(e){assert_string(e)});return Call.multi_foreach(d,{call:function(e){return new Call(a,e)},finish:function(){return BiwaScheme.undef}})});define_libfunc("string-copy",1,1,function(a){assert_string(a[0]);return a[0]});define_libfunc("vector?",1,1,function(a){return(a[0] instanceof Array)&&(a[0].closure_p!==true)});define_libfunc("make-vector",1,2,function(a){assert_integer(a[0]);var d=new Array(a[0]);if(a.length==2){for(var c=0;c<a[0];c++){d[c]=a[1]}}return d});define_libfunc("vector",1,null,function(a){return a});define_libfunc("vector-length",1,1,function(a){assert_vector(a[0]);return a[0].length});define_libfunc("vector-ref",2,2,function(a){assert_vector(a[0]);assert_integer(a[1]);return a[0][a[1]]});define_libfunc("vector-set!",3,3,function(a){assert_vector(a[0]);assert_integer(a[1]);a[0][a[1]]=a[2];return BiwaScheme.undef});define_libfunc("vector->list",1,1,function(a){assert_vector(a[0]);return a[0].to_list()});define_libfunc("list->vector",1,1,function(a){assert_list(a[0]);return a[0].to_array()});define_libfunc("vector-fill!",2,2,function(a){assert_vector(a[0]);var d=a[0],e=a[1];for(var c=0;c<d.length;c++){d[c]=e}return d});define_libfunc("vector-map",2,null,function(e){var d=e.shift(),f=e;f.each(function(a){assert_vector(a)});var c=[];return Call.multi_foreach(f,{call:function(a){return new Call(d,a)},result:function(a){c.push(a)},finish:function(){return c}})});define_libfunc("vector-for-each",2,null,function(c){var a=c.shift(),d=c;d.each(function(e){assert_vector(e)});return Call.multi_foreach(d,{call:function(e){return new Call(a,e)},finish:function(){return BiwaScheme.undef}})});define_libfunc("apply",2,null,function(c){var a=c.shift(),e=c.pop(),d=c;d=d.concat(e.to_array());return new Call(a,d)});define_syntax("call-with-current-continuation",function(a){return new Pair(Sym("call/cc"),a.cdr)});define_libfunc("values",0,null,function(a){return new Values(a)});define_libfunc("call-with-values",2,2,function(c){var a=c[0],d=c[1];return new Call(a,[],function(f){var e=f[0];if(!(e instanceof Values)){throw new Error("values expected, but got "+to_write(e))}return new Call(d,e.content)})});var expand_qq=function(c,d){if(c instanceof Symbol||c===nil){return[Sym("quote"),c].to_list()}else{if(c instanceof Pair){var a=c.car;if(a instanceof Pair&&a.car===Sym("unquote-splicing")){var d=d-1;if(d==0){return[Sym("append"),c.car.cdr.car,expand_qq(c.cdr,d+1)].to_list()}else{return[Sym("cons"),[Sym("list"),Sym("unquote-splicing"),expand_qq(c.car.cdr.car,d)].to_list(),expand_qq(c.cdr,d+1)].to_list()}}else{if(a===Sym("unquote")){var d=d-1;if(d==0){return c.cdr.car}else{return[Sym("list"),[Sym("quote"),Sym("unquote")].to_list(),expand_qq(c.cdr.car,d)].to_list()}}else{if(a===Sym("quasiquote")){return[Sym("list"),Sym("quasiquote"),expand_qq(c.cdr.car,d+1)].to_list()}else{return[Sym("cons"),expand_qq(c.car,d),expand_qq(c.cdr,d)].to_list()}}}}else{if(c instanceof Array){throw new Bug("vector quasiquotation is not implemented yet")}else{return c}}}};define_syntax("quasiquote",function(a){return expand_qq(a.cdr.car,1)});define_syntax("unquote",function(a){throw new Error("unquote(,) must be inside quasiquote(`)")});define_syntax("unquote-splicing",function(a){throw new Error("unquote-splicing(,@) must be inside quasiquote(`)")});define_libfunc("string-upcase",1,1,function(a){assert_string(a[0]);return a[0].toUpperCase()});define_libfunc("string-downcase",1,1,function(a){assert_string(a[0]);return a[0].toLowerCase()});BiwaScheme.make_string_ci_function=function(a){return function(c){assert_string(c[0]);var e=c[0].toUpperCase();for(var d=1;d<c.length;d++){assert_string(c[d]);if(!a(e,c[d].toUpperCase())){return false}}return true}};define_libfunc("string-ci=?",2,null,make_string_ci_function(function(d,c){return d==c}));define_libfunc("string-ci<?",2,null,make_string_ci_function(function(d,c){return d<c}));define_libfunc("string-ci>?",2,null,make_string_ci_function(function(d,c){return d>c}));define_libfunc("string-ci<=?",2,null,make_string_ci_function(function(d,c){return d<=c}));define_libfunc("string-ci>=?",2,null,make_string_ci_function(function(d,c){return d>=c}));define_libfunc("find",2,2,function(d){var c=d[0],a=d[1];assert_list(a);return Call.foreach(a,{call:function(e){return new Call(c,[e.car])},result:function(f,e){if(f){return e.car}},finish:function(){return false}})});define_libfunc("for-all",2,null,function(d){var c=d.shift();var a=d;a.each(function(f){assert_list(f)});var e=true;return Call.multi_foreach(a,{call:function(f){return new Call(c,f.map(function(g){return g.car}))},result:function(f,g){if(f===false){return false}e=f},finish:function(){return e}})});define_libfunc("exists",2,null,function(d){var c=d.shift();var a=d;a.each(function(e){assert_list(e)});return Call.multi_foreach(a,{call:function(e){return new Call(c,e.map(function(f){return f.car}))},result:function(e,f){if(e!==false){return e}},finish:function(){return false}})});define_libfunc("filter",2,2,function(f){var e=f[0],d=f[1];assert_list(d);var c=[];return Call.foreach(d,{call:function(a){return new Call(e,[a.car])},result:function(g,a){if(g){c.push(a.car)}},finish:function(){return c.to_list()}})});define_libfunc("partition",2,2,function(d){var c=d[0],a=d[1];assert_list(a);var e=[],g=[];return Call.foreach(a,{call:function(f){return new Call(c,[f.car])},result:function(h,f){if(h){e.push(f.car)}else{g.push(f.car)}},finish:function(){return new Values([e.to_list(),g.to_list()])}})});define_libfunc("fold-left",3,null,function(e){var c=e.shift(),d=e.shift(),a=e;a.each(function(f){assert_list(f)});return Call.multi_foreach(a,{call:function(g){var f=g.map(function(h){return h.car});f.unshift(d);return new Call(c,f)},result:function(f,g){d=f},finish:function(){return d}})});define_libfunc("fold-right",3,null,function(e){var c=e.shift(),d=e.shift();var a=e.map(function(f){assert_list(f);return f.to_array().reverse().to_list()});return Call.multi_foreach(a,{call:function(g){var f=g.map(function(h){return h.car});f.push(d);return new Call(c,f)},result:function(f,g){d=f},finish:function(){return d}})});define_libfunc("remp",2,2,function(d){var c=d[0],a=d[1];assert_list(a);var e=[];return Call.foreach(a,{call:function(f){return new Call(c,[f.car])},result:function(g,f){if(!g){e.push(f.car)}},finish:function(){return e.to_list()}})});var make_remover=function(a){return function(d){var f=d[0],c=d[1];assert_list(c);var e=[];return Call.foreach(c,{call:function(g){return new Call(TopEnv[a]||CoreEnv[a],[f,g.car])},result:function(h,g){if(!h){e.push(g.car)}},finish:function(){return e.to_list()}})}};define_libfunc("remove",2,2,make_remover("equal?"));define_libfunc("remv",2,2,make_remover("eqv?"));define_libfunc("remq",2,2,make_remover("eq?"));define_libfunc("memp",2,2,function(d){var c=d[0],a=d[1];assert_list(a);var e=[];return Call.foreach(a,{call:function(f){return new Call(c,[f.car])},result:function(g,f){if(g){return f}},finish:function(){return false}})});var make_finder=function(a){return function(d){var f=d[0],c=d[1];assert_list(c);var e=[];return Call.foreach(c,{call:function(g){return new Call(TopEnv[a]||CoreEnv[a],[f,g.car])},result:function(h,g){if(h){return g}},finish:function(){return false}})}};define_libfunc("member",2,2,make_finder("equal?"));define_libfunc("memv",2,2,make_finder("eqv?"));define_libfunc("memq",2,2,make_finder("eq?"));define_libfunc("assp",2,2,function(c){var a=c[0],e=c[1];assert_list(e);var d=[];return Call.foreach(e,{call:function(f){if(f.car.car){return new Call(a,[f.car.car])}else{throw new Error("ass*: pair required but got "+to_write(f.car))}},result:function(g,f){if(g){return f.car}},finish:function(){return false}})});var make_assoc=function(a){return function(d){var f=d[0],c=d[1];assert_list(c);var e=[];return Call.foreach(c,{call:function(g){if(g.car.car){return new Call(TopEnv[a]||CoreEnv[a],[f,g.car.car])}else{throw new Error("ass*: pair required but got "+to_write(g.car))}},result:function(h,g){if(h){return g.car}},finish:function(){return false}})}};define_libfunc("assoc",2,2,make_assoc("equal?"));define_libfunc("assv",2,2,make_assoc("eqv?"));define_libfunc("assq",2,2,make_assoc("eq?"));define_libfunc("cons*",1,null,function(a){if(a.length==1){return a[0]}else{var c=null;a.reverse().each(function(d){if(c){c=new Pair(d,c)}else{c=d}});return c}});define_libfunc("list-sort",1,2,function(a){if(a[1]){throw new Bug("list-sort: cannot take compare proc now")}assert_list(a[0]);return a[0].to_array().sort().to_list()});define_libfunc("vector-sort",1,2,function(a){if(a[1]){throw new Bug("vector-sort: cannot take compare proc now")}assert_vector(a[0]);return a[0].clone().sort()});define_libfunc("vector-sort!",1,2,function(a){if(a[1]){throw new Bug("vector-sort!: cannot take compare proc now")}assert_vector(a[0]);a[0].sort();return BiwaScheme.undef});define_syntax("when",function(c){var d=c.cdr.car,a=c.cdr.cdr;return new Pair(Sym("if"),new Pair(d,new Pair(new Pair(Sym("begin"),a),new Pair(BiwaScheme.undef,nil))))});define_syntax("unless",function(c){var d=c.cdr.car,a=c.cdr.cdr;return new Pair(Sym("if"),new Pair(new Pair(Sym("not"),new Pair(d,nil)),new Pair(new Pair(Sym("begin"),a),new Pair(BiwaScheme.undef,nil))))});define_syntax("do",function(h){if(!BiwaScheme.isPair(h.cdr)){throw new Error("do: no variables of do")}var l=h.cdr.car;if(!BiwaScheme.isPair(l)){throw new Error("do: variables must be given as a list")}if(!BiwaScheme.isPair(h.cdr.cdr)){throw new Error("do: no resulting form of do")}var a=h.cdr.cdr.car;var k=h.cdr.cdr.cdr;var d=BiwaScheme.gensym();var e=l.map(function(o){var m=o.to_array();return List(m[0],m[1])}).to_list();var f=a.car;var g=new Pair(Sym("begin"),a.cdr);var c=new Pair(d,l.map(function(o){var m=o.to_array();return m[2]||m[0]}).to_list());var j=new Pair(Sym("begin"),k).concat(List(c));return List(Sym("let"),d,e,List(Sym("if"),f,g,j))});define_syntax("case-lambda",function(a){});define_syntax("define-record-type",function(o){var u=o.cdr.car;var y=o.cdr.cdr;if(BiwaScheme.isSymbol(u)){var f=u;var B=Sym("make-"+u.name);var d=Sym(u.name+"?")}else{assert_list(u);var f=u.car;var B=u.cdr.car;var d=u.cdr.cdr.car;assert_symbol(f);assert_symbol(B);assert_symbol(d)}var A=false;var k=false;var a=false;var j=false;var g;var c=false;var t=false;var r=false;var v=[];y.to_array().each(function(x){switch(x.car){case Sym("fields"):v=x.cdr.to_array().map(function(D,C){if(BiwaScheme.isSymbol(D)){return{name:D,idx:C,mutable:false,accessor_name:null,mutator_name:null}}else{assert_list(D);assert_symbol(D.car);switch(D.car){case Sym("immutable"):var E=D.cdr.car;assert_symbol(E);if(BiwaScheme.isNil(D.cdr.cdr)){return{name:E,idx:C,mutable:false}}else{return{name:E,idx:C,mutable:false,accessor_name:D.cdr.cdr.car}}break;case Sym("mutable"):var E=D.cdr.car;assert_symbol(E);if(BiwaScheme.isNil(D.cdr.cdr)){return{name:E,idx:C,mutable:true}}else{return{name:E,idx:C,mutable:true,accessor_name:D.cdr.cdr.car,mutator_name:D.cdr.cdr.cdr.car}}break;default:throw new Error("define-record-type: field definition must start with `immutable' or 'mutable'")}}});break;case Sym("parent"):g=x.cdr.car;assert_symbol(g);break;case Sym("protocol"):r=x.cdr.car;break;case Sym("sealed"):A=!!x.cdr.car;break;case Sym("opaque"):k=!!x.cdr.car;break;case Sym("nongenerative"):a=true;j=x.cdr.car;break;case Sym("parent-rtd"):c=x.cdr.car;t=x.cdr.cdr.car;break;default:throw new BiwaScheme.Error("define-record-type: unknown clause `"+BiwaScheme.to_write(x.car)+"'")}});if(g){c=[Sym("record-type-descriptor"),g];t=[Sym("record-constructor-descriptor"),g]}var m=[Sym("record-type-descriptor"),f];var s=[Sym("record-constructor-descriptor"),f];var z=v.map(function(x){return BiwaScheme.build_list([Sym(x.mutable?"mutable":"immutable"),x.name])});z.is_vector=true;var w=[Sym("make-record-type-descriptor"),[Sym("quote"),f],c,j,A,k,z];var q=[Sym("make-record-constructor-descriptor"),Sym("__rtd"),t,r];var h=[Sym("let*"),[[Sym("__rtd"),w],[Sym("__cd"),q]],[Sym("_define-record-type"),[Sym("quote"),f],Sym("__rtd"),Sym("__cd")]];var l=v.map(function(C){var x=C.accessor_name||Sym(f.name+"-"+C.name.name);return[Sym("define"),x,[Sym("record-accessor"),m,C.idx]]});var e=v.findAll(function(x){return x.mutable}).map(function(C){var x=C.mutator_name||Sym("set-"+f.name+"-"+C.name.name+"!");return[Sym("define"),x,[Sym("record-mutator"),m,C.idx]]});return BiwaScheme.build_list([Sym("begin"),h,[Sym("define"),B,[Sym("record-constructor"),s]],[Sym("define"),d,[Sym("record-predicate"),m]],].concat(l).concat(e))});define_libfunc("_define-record-type",3,3,function(a){assert_symbol(a[0]);assert_record_td(a[1]);assert_record_cd(a[2]);BiwaScheme.Record.define_type(a[0].name,a[1],a[2]);return BiwaScheme.undef});define_syntax("record-type-descriptor",function(a){return BiwaScheme.build_list([Sym("_record-type-descriptor"),[Sym("quote"),a.cdr.car]])});define_libfunc("_record-type-descriptor",1,1,function(a){assert_symbol(a[0]);var c=BiwaScheme.Record.get_type(a[0].name);if(c){return c.rtd}else{throw new Error("record-type-descriptor: unknown record type "+a[0].name)}});define_syntax("record-constructor-descriptor",function(a){return BiwaScheme.build_list([Sym("_record-constructor-descriptor"),[Sym("quote"),a.cdr.car]])});define_libfunc("_record-constructor-descriptor",1,1,function(a){assert_symbol(a[0]);var c=BiwaScheme.Record.get_type(a[0].name);if(c){return c.cd}else{throw new Error("record-constructor-descriptor: unknown record type "+a[0].name)}});define_libfunc("make-record-type-descriptor",6,6,function(g){var c=g[0],d=g[1],l=g[2],e=g[3],j=g[4],k=g[5];assert_symbol(c);if(d){assert_record_td(d)}if(l){assert_symbol(l);var f=BiwaScheme.Record.RTD.NongenerativeRecords.get(l.name);if(f){return f}}e=!!e;j=!!j;assert_vector(k);for(var h=0;h<k.length;h++){var m=k[h];assert_symbol(m.car,"mutability");assert_symbol(m.cdr.car,"field name");k[h]=[m.cdr.car.name,(m.car==Sym("mutable"))]}var a=new BiwaScheme.Record.RTD(c,d,l,e,j,k);if(l){BiwaScheme.Record.RTD.NongenerativeRecords.set(l.name,a)}return a});define_libfunc("record-type-descriptor?",1,1,function(a){return(a[0] instanceof BiwaScheme.Record.RTD)});define_libfunc("make-record-constructor-descriptor",3,3,function(a){var c=a[0],d=a[1],e=a[2];assert_record_td(c);if(d){assert_record_cd(d)}if(e){assert_procedure(e)}return new BiwaScheme.Record.CD(c,d,e)});define_libfunc("record-constructor",1,1,function(a){var c=a[0];assert_record_cd(c);return c.record_constructor()});define_libfunc("record-predicate",1,1,function(a){var c=a[0];assert_record_td(c);return function(d){var e=d[0];return(e instanceof BiwaScheme.Record)&&(e.rtd===c)}});define_libfunc("record-accessor",2,2,function(c){var d=c[0],a=c[1];assert_record_td(d);assert_integer(a);for(var e=d.parent_rtd;e;e=e.parent_rtd){a+=e.fields.length}return function(g){var f=g[0];assert_record(f);var h=false;for(var j=f.rtd;j;j=j.parent_rtd){if(j==d){h=true}}assert(h,"(record-accessor): "+BiwaScheme.to_write(f)+" is not a "+d.name);return f.get(a)}});define_libfunc("record-mutator",2,2,function(c){var d=c[0],a=c[1];assert_record_td(d);assert_integer(a);for(var e=d.parent_rtd;e;e=e.parent_rtd){a+=e.fields.length}return function(g){var f=g[0],h=g[1];assert_record(f);assert(f.rtd===d,"(record-mutator): "+BiwaScheme.to_write(f)+" is not a "+d.name);assert(!f.rtd.sealed,"(record-mutator): "+d.name+" is sealed (can't mutate)");f.set(a,h)}});define_libfunc("record?",1,1,function(a){var c=a[0];if(BiwaScheme.isRecord(c)){if(c.rtd.opaque){return false}else{return true}}else{return false}});define_libfunc("record-rtd",1,1,function(a){assert_record(a[0]);return a[0].rtd});define_libfunc("record-type-name",1,1,function(a){assert_record_td(a[0]);return a[0].name});define_libfunc("record-type-parent",1,1,function(a){assert_record_td(a[0]);return a[0].parent_rtd});define_libfunc("record-type-uid",1,1,function(a){assert_record_td(a[0]);return a[0].uid});define_libfunc("record-type-generative?",1,1,function(a){assert_record_td(a[0]);return a[0].generative});define_libfunc("record-type-sealed?",1,1,function(a){assert_record_td(a[0]);return a[0].sealed});define_libfunc("record-type-opaque?",1,1,function(a){assert_record_td(a[0]);return a[0].opaque});define_libfunc("record-type-field-names",1,1,function(a){assert_record_td(a[0]);return a[0].fields.map(function(c){return c.name})});define_libfunc("record-field-mutable?",2,2,function(c){var d=c[0],a=c[1];assert_record_td(c[0]);assert_integer(a);for(var e=d.parent_rtd;e;e=e.parent_rtd){a+=e.fields.length}return c[0].fields[a].mutable});define_libfunc("raise",1,1,function(a){throw new BiwaScheme.UserError(BiwaScheme.to_write(a[0]))});define_libfunc("port?",1,1,function(a){return(a[0] instanceof Port)});define_libfunc("textual-port?",1,1,function(a){assert_port(a[0]);return !a[0].is_binary});define_libfunc("binary-port?",1,1,function(a){assert_port(a[0]);return a[0].is_binary});define_libfunc("close-port",1,1,function(a){assert_port(a[0]);a[0].close();return BiwaScheme.undef});define_libfunc("call-with-port",2,2,function(d){var c=d[0],a=d[1];assert_port(c);assert_closure(a);return new Call(a,[c],function(e){c.close();return e[0]})});define_libfunc("put-char",2,2,function(a){assert_port(a[0]);assert_char(a[1]);a[0].put_string(a[1].value);return BiwaScheme.undef});define_libfunc("put-string",2,2,function(a){assert_port(a[0]);assert_string(a[1]);a[0].put_string(a[1]);return BiwaScheme.undef});define_libfunc("put-datum",2,2,function(a){assert_port(a[0]);a[0].put_string(to_write(a[1]));return BiwaScheme.undef});define_libfunc("eof-object",0,0,function(a){return eof});define_libfunc("eof-object?",1,1,function(a){return a[0]===eof});define_libfunc("input-port?",1,1,function(a){assert_port(a[0]);return a[0].is_input});define_libfunc("output-port?",1,1,function(a){assert_port(a[0]);return a[0].is_output});define_libfunc("current-input-port",0,0,function(a){return Port.current_input});define_libfunc("current-output-port",0,0,function(a){return Port.current_output});define_libfunc("current-error-port",0,0,function(a){return Port.current_error});define_libfunc("close-input-port",1,1,function(a){assert_port(a[0]);if(!a[0].is_input){throw new Error("close-input-port: port is not input port")}a[0].close();return BiwaScheme.undef});define_libfunc("close-output-port",1,1,function(a){assert_port(a[0]);if(!a[0].is_output){throw new Error("close-output-port: port is not output port")}a[0].close();return BiwaScheme.undef});define_libfunc("read",0,1,function(c){var a=c[0]||Port.current_input;assert_port(a);return a.get_string(function(d){return Interpreter.read(d)})});define_libfunc("newline",0,1,function(c){var a=c[0]||Port.current_output;a.put_string("\n");return BiwaScheme.undef});define_libfunc("display",1,2,function(c){var a=c[1]||Port.current_output;a.put_string(to_display(c[0]));return BiwaScheme.undef});define_libfunc("write",1,2,function(c){var a=c[1]||Port.current_output;assert_port(a);a.put_string(to_write(c[0]));return BiwaScheme.undef});define_libfunc("file-exists?",1,1,function(c){netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");assert_string(c[0]);var a=FileIO.open(c[0]);return a.exists()});define_libfunc("delete-file",1,1,function(c){netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect");assert_string(c[0]);var a=FileIO.unlink(FileIO.open(c[0]));if(!a){puts("delete-file: cannot delete "+c[0])}return BiwaScheme.undef});define_libfunc("make-eq-hashtable",0,1,function(a){return new Hashtable(Hashtable.eq_hash,Hashtable.eq_equiv)});define_libfunc("make-eqv-hashtable",0,1,function(a){return new Hashtable(Hashtable.eqv_hash,Hashtable.eqv_equiv)});define_libfunc("make-hashtable",2,3,function(a){assert_procedure(a[0]);assert_procedure(a[1]);return new Hashtable(a[0],a[1])});define_libfunc("hashtable?",1,1,function(a){return a[0] instanceof Hashtable});define_libfunc("hashtable-size",1,1,function(a){assert_hashtable(a[0]);return a[0].keys().length});BiwaScheme.find_hash_pair=function(d,a,c){return new Call(d.hash_proc,[a],function(e){var g=e[0];var f=d.candidate_pairs(g);if(!f){return c.on_not_found(g)}return Call.foreach(f,{call:function(h){return new Call(d.equiv_proc,[a,h[0]])},result:function(h,j){if(h){return c.on_found(j,g)}},finish:function(){return c.on_not_found(g)}})})};define_libfunc("hashtable-ref",3,3,function(a){var e=a[0],c=a[1],d=a[2];assert_hashtable(e);return BiwaScheme.find_hash_pair(e,c,{on_found:function(f){return f[1]},on_not_found:function(f){return d}})});define_libfunc("hashtable-set!",3,3,function(a){var e=a[0],c=a[1],d=a[2];assert_hashtable(e);return BiwaScheme.find_hash_pair(e,c,{on_found:function(f){f[1]=d;return BiwaScheme.undef},on_not_found:function(f){e.add_pair(f,c,d);return BiwaScheme.undef}})});define_libfunc("hashtable-delete!",2,2,function(a){var d=a[0],c=a[1];assert_hashtable(d);return BiwaScheme.find_hash_pair(d,c,{on_found:function(f,e){d.remove_pair(e,f);return BiwaScheme.undef},on_not_found:function(e){return BiwaScheme.undef}})});define_libfunc("hashtable-contains?",2,2,function(a){var d=a[0],c=a[1];assert_hashtable(d);return BiwaScheme.find_hash_pair(d,c,{on_found:function(e){return true},on_not_found:function(e){return false}})});define_libfunc("hashtable-update!",4,4,function(c){var f=c[0],d=c[1],a=c[2],e=c[3];assert_hashtable(f);assert_procedure(a);return BiwaScheme.find_hash_pair(f,d,{on_found:function(h,g){return new Call(a,[h[1]],function(j){h[1]=j[0];return BiwaScheme.undef})},on_not_found:function(g){return new Call(a,[e],function(h){f.add_pair(g,d,h[0]);return BiwaScheme.undef})}})});define_libfunc("hashtable-copy",1,2,function(c){var a=(c[1]===undefined)?false:!!c[1];assert_hashtable(c[0]);return c[0].create_copy(a)});define_libfunc("hashtable-clear!",0,1,function(a){assert_hashtable(a[0]);a[0].clear();return BiwaScheme.undef});define_libfunc("hashtable-keys",1,1,function(a){assert_hashtable(a[0]);return a[0].keys()});define_libfunc("hashtable-entries",1,1,function(a){assert_hashtable(a[0]);return new Values([a[0].keys(),a[0].values()])});define_libfunc("hashtable-equivalence-function",1,1,function(a){assert_hashtable(a[0]);return a[0].equiv_proc});define_libfunc("hashtable-hash-function",1,1,function(a){assert_hashtable(a[0]);return a[0].hash_proc});define_libfunc("hashtable-mutable?",1,1,function(a){assert_hashtable(a[0]);return a[0].mutable});define_libfunc("equal-hash",0,0,function(a){return Hashtable.equal_hash});define_libfunc("string-hash",0,0,function(a){return Hashtable.string_hash});define_libfunc("string-ci-hash",0,0,function(a){return Hashtable.string_ci_hash});define_libfunc("symbol-hash",0,0,function(a){return Hashtable.symbol_hash});define_libfunc("eval",1,1,function(c,a){var d=c[0];var a=new BiwaScheme.Interpreter(a.on_error);return a.evaluate(d.to_write())})}if(typeof(BiwaScheme)!="object"){BiwaScheme={}}with(BiwaScheme){define_libfunc("read-line",0,1,function(c){var a=c[0]||Port.current_input;assert_port(a);return a.get_string()});define_libfunc("element-clear!",1,1,function(a){return $(a[0]).update()});define_libfunc("element-empty!",1,1,function(a){return $(a[0]).update()});define_libfunc("element-visible?",1,1,function(a){return $(a[0]).visible()});define_libfunc("element-toggle!",1,1,function(a){return $(a[0]).toggle()});define_libfunc("element-hide!",1,1,function(a){return $(a[0]).hide()});define_libfunc("element-show!",1,1,function(a){return $(a[0]).show()});define_libfunc("element-remove!",1,1,function(a){return $(a[0]).remove("")});define_libfunc("element-update!",2,2,function(a){return $(a[0]).update(a[1])});define_libfunc("element-replace!",2,2,function(a){return $(a[0]).replace(a[1])});define_libfunc("element-insert!",2,2,function(a){return $(a[0]).insert(a[1])});define_libfunc("element-wrap!",3,3,function(a){throw new Bug("not yet implemented")});define_libfunc("element-ancestors",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-descendants",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-first-descendant",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-immediate-descendants",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-previous-sibling",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-next-sibling",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-siblings",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-match?",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-up",3,3,function(a){throw new Bug("not yet implemented")});define_libfunc("element-down",3,3,function(a){throw new Bug("not yet implemented")});define_libfunc("element-previous",3,3,function(a){throw new Bug("not yet implemented")});define_libfunc("element-next",3,3,function(a){throw new Bug("not yet implemented")});define_libfunc("element-select",0,0,function(a){throw new Bug("not yet implemented")});define_libfunc("element-adjacent",0,0,function(a){throw new Bug("not yet implemented")});define_libfunc("element-identify",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-read-attribute",2,2,function(a){assert_string(a[1]);return $(a[0]).readAttribute(a[1])});define_libfunc("element-write-attribute",3,3,function(a){assert_string(a[1]);return $(a[0]).writeAttribute(a[1],a[2])});define_libfunc("element-height",1,1,function(a){return $(a[0]).getHeight()});define_libfunc("element-width",1,1,function(a){return $(a[0]).getWidth()});define_libfunc("element-class-names",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-has-class-name?",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-add-class-name",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-remove-class-name",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-toggle-class-name",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-clean-whitespace!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-empty?",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-descendant-of!",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("scroll-to-element!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-style",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-opacity",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-style-set!",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-opacity-set!",2,2,function(a){throw new Bug("not yet implemented")});define_libfunc("element-dimensions",1,1,function(a){var c=$(a[0]).getDimensions();return new Values(c.width,c.height)});define_libfunc("element-make-positioned!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-undo-positioned!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-make-clipping!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-undo-clipping!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-cumulative-offset",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-positioned-offset",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-absolutize!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-relativize!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-cumulative-scroll-offset",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-offset-parent",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-viewport-offset",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-clone-position!",1,1,function(a){throw new Bug("not yet implemented")});define_libfunc("element-absolutize!",1,1,function(a){throw new Bug("not yet implemented")});BiwaScheme.create_elements_by_dom=function(c){var a=function(k,j,l){var h=new Element(k,j);l.each(function(m){h.insert({bottom:m})});return h};var c=c.to_array();var e=c[0].name||c[0];var d={};var g=[];for(var f=1;f<c.length;f++){if(c[f] instanceof Symbol){d[c[f].name]=c[f+1];f++}else{if(c[f] instanceof Pair){g.push(create_elements_by_dom(c[f]))}else{g.push(c[f].toString())}}}return a(e,d,g)};BiwaScheme.create_elements_by_string=function(a){var a=a.to_array();var c=a.shift();if(c instanceof Symbol){c=c.name}if(c.match(/(.*)\.(.*)/)){c=RegExp.$1;a.unshift(Sym("class"),RegExp.$2)}if(c.match(/(.*)\#(.*)/)){c=RegExp.$1;a.unshift(Sym("id"),RegExp.$2)}var e=[];var f=["<"+c];for(var d=0;d<a.length;d++){if(a[d] instanceof Symbol){f.push(" "+a[d].name+'="'+a[d+1]+'"');d++}else{if(a[d] instanceof Pair){e.push(create_elements_by_string(a[d]))}else{e.push(a[d])}}}f.push(">");f.push(e.join(""));f.push("</"+c+">");return f.join("")};BiwaScheme.tree_all=function(a,c){if(a===nil){return true}else{if(c(a.car)===false){return false}else{return BiwaScheme.tree_all(a.cdr,c)}}};define_libfunc("element-new",1,1,function(a){var c=function(e){return Object.isString(e)||(e instanceof Symbol)||(e instanceof Pair)};if(BiwaScheme.tree_all(a[0],c)){var d=new Element("div");d.update(create_elements_by_string(a[0]));return d.firstChild}else{return nil}});define_libfunc("element-content",1,1,function(a){return a[0].value||(a[0].innerHTML).unescapeHTML()});define_libfunc("load",1,1,function(c,a){var d=c[0];assert_string(d);return new BiwaScheme.Pause(function(e){new Ajax.Request(d,{method:"get",evalJSON:false,evalJS:false,onSuccess:function(g){var f=new Interpreter(a.on_error);f.evaluate(g.responseText,function(){return e.resume(BiwaScheme.undef)})},onFailure:function(f){throw new Error("load: network error: failed to load"+d)}})})});_require=function(f,c,a){var d=document.createElement("script");d.src=f;document.body.appendChild(d);var e=new Function("return !!("+c+")");if(e()){a()}else{setTimeout(function(){e()?a():setTimeout(arguments.callee,10)},10)}};define_libfunc("js-load",2,2,function(c){var d=c[0];var a=c[1];assert_string(d);assert_string(a);return new BiwaScheme.Pause(function(e){_require(d,"window."+a,function(){e.resume(BiwaScheme.undef)})})});BiwaScheme.getelem=function(c){var a=$(c[0]);if(a===undefined||a===null){return false}else{return a}};define_libfunc("$",1,1,BiwaScheme.getelem);define_libfunc("getelem",1,1,BiwaScheme.getelem);define_libfunc("set-style!",3,3,function(a){assert_string(a[1]);a[0].style[a[1]]=a[2];return BiwaScheme.undef});define_libfunc("get-style",2,2,function(a){assert_string(a[1]);return a[0].style[a[1]]});define_libfunc("set-content!",2,2,function(a){assert_string(a[1]);var c=a[1].replace(/\n/g,"<br>").replace(/\t/g,"&nbsp;&nbsp;&nbsp;");a[0].innerHTML=c;return BiwaScheme.undef});define_libfunc("get-content",1,1,function(a){return a[0].value||(a[0].innerHTML).unescapeHTML()});define_libfunc("timer",2,2,function(d,c){var a=d[0],e=d[1];assert_closure(a);assert_real(e);setTimeout(function(){(new Interpreter(c.on_error)).invoke_closure(a)},e*1000);return BiwaScheme.undef});define_libfunc("set-timer!",2,2,function(d,c){var a=d[0],e=d[1];assert_closure(a);assert_real(e);return setInterval(function(){(new Interpreter(c.on_error)).invoke_closure(a)},e*1000)});define_libfunc("clear-timer!",1,1,function(a){var c=a[0];clearInterval(c);return BiwaScheme.undef});define_libfunc("sleep",1,1,function(a){var c=a[0];assert_real(c);return new BiwaScheme.Pause(function(d){setTimeout(function(){d.resume(nil)},c*1000)})});define_libfunc("set-handler!",3,3,function(c,a){throw new Error("set-handler! is obsolete, please use add-handler! instead")});define_libfunc("add-handler!",3,3,function(e,d){var f=e[0],g=e[1],a=e[2];var c=d.on_error;Event.observe(f,g,function(j){var h=new Interpreter(c);h.invoke_closure(a,[j||Window.event])});return BiwaScheme.undef});define_libfunc("wait-for",2,2,function(a){var c=a[0],e=a[1];c.biwascheme_wait_for=c.biwascheme_wait_for||{};var d=c.biwascheme_wait_for[e];if(d){Event.stopObserving(c,e,d)}return new BiwaScheme.Pause(function(g){var f=function(h){c.biwascheme_wait_for[e]=undefined;Event.stopObserving(c,e,f);return g.resume(BiwaScheme.undef)};c.biwascheme_wait_for[e]=f;Event.observe(c,e,f)})});define_libfunc("domelem",1,null,function(a){throw new Error("obsolete")});define_libfunc("dom-remove-children!",1,1,function(a){puts("warning: dom-remove-children! is obsolete. use element-empty! instead");$(a[0]).update("");return BiwaScheme.undef});define_libfunc("dom-create-element",1,1,function(a){throw new Error("obsolete")});define_libfunc("element-append-child!",2,2,function(a){return $(a[0]).appendChild(a[1])});define_libfunc("dom-remove-child!",2,2,function(a){throw new Error("obsolete")});define_libfunc_raw("js-eval",1,1,function(ar){return eval(ar[0])});define_libfunc_raw("js-ref",2,2,function(a){assert_string(a[1]);return a[0][a[1]]});define_libfunc("js-set!",3,3,function(a){assert_string(a[1]);a[0][a[1]]=a[2];return BiwaScheme.undef});define_libfunc_raw("js-call",1,null,function(a){var c=a.shift();assert_function(c);var d=null;return c.apply(d,a)});define_libfunc_raw("js-invoke",2,null,function(d){var c=d.shift();var a=d.shift();assert_string(a);if(c[a]){return c[a].apply(c,d)}else{throw new Error("js-invoke: function "+a+" is not defined")}});define_libfunc("js-new",1,null,function(ar,intp){var array_to_obj=function(ary){if((ary.length%2)!=0){throw new Error("js-new: odd number of key-value pair")}var obj={};for(var i=0;i<ary.length;i+=2){var key=ary[i],value=ary[i+1];assert_symbol(key);if(value.closure_p===true){value=BiwaScheme.js_closure(value,intp)}obj[key.name]=value}return obj};var ctor=ar.shift();assert_string(ctor);if(ar.length==0){return eval("new "+ctor+"()")}else{var args=[];for(var i=0;i<ar.length;i++){if(ar[i] instanceof Symbol){args.push(array_to_obj(ar.slice(i)));break}else{args.push(ar[i])}}var args_str=ar.map(function(value,i){return"args['"+i+"']"}).join(",");return eval("new "+ctor+"("+args_str+")")}});define_libfunc("js-obj",0,null,function(a){if(a.length%2!=0){throw new Error("js-obj: number of arguments must be even")}var c={};for(i=0;i<a.length/2;i++){assert_string(a[i*2]);c[a[i*2]]=a[i*2+1]}return c});BiwaScheme.js_closure=function(a,d){var c=d.on_error;return function(){var e=new Interpreter(c);e.invoke_closure(a,$A(arguments))}};define_libfunc("js-closure",1,1,function(c,a){assert_closure(c[0]);return BiwaScheme.js_closure(c[0],a)});define_libfunc("js-null?",1,1,function(a){return a[0]===null});define_libfunc("js-undefined?",1,1,function(a){return a[0]===undefined});define_libfunc("http-request",1,1,function(a){var c=a[0];assert_string(c);return new BiwaScheme.Pause(function(d){new Ajax.Request(c,{method:"get",onSuccess:function(e){d.resume(e.responseText)}})})});define_libfunc("http-post",2,2,function(a){var e=a[0];assert_string(e);var d=a[1];assert_list(d);var c={};d.foreach(function(f){assert_string(f.car);c[f.car]=f.cdr});return new BiwaScheme.Pause(function(f){new Ajax.Request(e,{method:"post",postBody:$H(c).toQueryString(),onSuccess:function(g){f.resume(g.responseText)}})})});BiwaScheme.jsonp_receiver=[];define_libfunc("receive-jsonp",1,1,function(a){var d=a[0];assert_string(d);var f=BiwaScheme.jsonp_receiver;for(var e=0;e<f.length;e++){if(f[e]===null){break}}var c=e;d+="?callback=BiwaScheme.jsonp_receiver["+c+"]";return new BiwaScheme.Pause(function(h){f[c]=function(j){h.resume(j);f[c]=null};var g=document.createElement("script");g.src=d;document.body.appendChild(g)})});define_libfunc("alert",1,1,function(a){alert(a[0]);return BiwaScheme.undef});define_libfunc("confirm",1,1,function(a){return confirm(a[0])});BiwaScheme.TupleSpaceClient=Class.create({initialize:function(a){this.server_path=a},nonblocking_request:function(a,c){var d=this.server_path+a+"?"+encodeURIComponent(to_write(c));return this.connect(d)},blocking_request:function(a,c){this.assert_init();var d=this.server_path+a+"?"+encodeURIComponent(to_write(c))+"&cid="+this.client_id;return new BiwaScheme.Pause(function(e){this.ajax(d,function(f){this.observe(f,function(g){e.resume(g)})}.bind(this))}.bind(this))},write:function(a){return this.nonblocking_request("write",a)},readp:function(a){return this.nonblocking_request("readp",a)},takep:function(a){return this.nonblocking_request("takep",a)},dump:function(){return this.nonblocking_request("dump","")},read:function(a){return this.blocking_request("read",a)},take:function(a){return this.blocking_request("take",a)},ajax:function(a,c){new Ajax.Request(a,{onSuccess:function(d){c(d.responseText)},onFailure:function(){puts("error: failed to access "+a)}})},init_connection:function(){if(this.client_id){return this.client_id}else{return new Pause(function(c){var a=this.server_path+"init_connection";this.ajax(a,function(d){this.client_id=d;this.start_connection();c.resume(this.client_id)}.bind(this))}.bind(this))}},assert_init:function(){if(!this.client_id){puts("ts-init not called:"+Object.inspect(this.client_id));throw new Error("ts-init not called")}},start_connection:function(){var c=this.server_path+"connection?cid="+this.client_id;var a=function(){this.ajax(c,function(g){var e=Interpreter.read(g);if(e){var f=e.car,d=e.cdr;this.notify(f,d);a()}}.bind(this))}.bind(this);a()},waiters:[],too_early_tuples:[],observe:function(a,c){if(this.too_early_tuples[a]){c(this.too_early_tuples[a]);this.too_early_tuples[a]=undefined}else{if(this.waiters[a]){puts("Bug: ticket conflicted")}else{this.waiters[a]=c}}},notify:function(c,a){var d=this.waiters[c];if(d){this.waiters[c]=undefined;return d(a)}else{this.too_early_tuples[c]=a}},connect:function(c,a){c+="&time="+(new Date()).getTime();return new BiwaScheme.Pause(function(d){new Ajax.Request(c,{method:"get",onSuccess:function(f){var e=f.responseText;if(a){e=a(e)}else{e=Interpreter.read(e)}if(e==undefined){d.resume(false)}else{d.resume(e)}},onFailure:function(e){throw new Error("ts_client.connect: failed to access"+c);d.resume(false)}})})}});BiwaScheme.ts_client=new TupleSpaceClient("/ts/");define_libfunc("ts-init",0,0,function(a){return ts_client.init_connection()});define_libfunc("ts-write",1,1,function(a){return ts_client.write(a[0])});define_libfunc("ts-read",1,1,function(a){var c=a[0];return ts_client.read(c)});define_libfunc("ts-readp",1,1,function(a){return ts_client.readp(a[0])});define_libfunc("ts-take",1,1,function(a){return ts_client.take(a[0])});define_libfunc("ts-takep",1,1,function(a){return ts_client.takep(a[0])});define_libfunc("ts-dump",0,0,function(a){return ts_client.dump()})}if(typeof(BiwaScheme)!="object"){BiwaScheme={}}with(BiwaScheme){define_libfunc("html-escape",1,1,function(a){assert_string(a[0]);return a[0].escapeHTML()});BiwaScheme.inspect_objs=function(a){return a.map(function(c){if(c.inspect){return c.inspect()}else{return Object.inspect($H(c))}}).join(", ")};define_libfunc("inspect",1,null,function(a){return BiwaScheme.inspect_objs(a)});define_libfunc("inspect!",1,null,function(a){puts(BiwaScheme.inspect_objs(a));return BiwaScheme.undef});BiwaScheme.json2sexp=function(c){switch(true){case Object.isNumber(c)||Object.isString(c)||c===true||c===false:return c;case Object.isArray(c):return c.map(function(d){return json2sexp(d)}).to_list();case typeof(c)=="object":var a=nil;for(key in c){a=new Pair(new Pair(key,json2sexp(c[key])),a)}return a;default:throw new Error("json->sexp: detected invalid value for json: "+Object.inspect(c))}throw new Bug("must not happen")};define_libfunc("json->sexp",1,1,function(a){return json2sexp(a[0])});define_libfunc("identity",1,1,function(a){return a[0]});define_libfunc("string-concat",1,1,function(a){assert_list(a[0]);return a[0].to_array().join("")});define_libfunc("string-split",2,2,function(a){assert_string(a[0]);assert_string(a[1]);return a[0].split(a[1]).to_list()});define_libfunc("string-join",1,2,function(a){assert_list(a[0]);var c="";if(a[1]){assert_string(a[1]);c=a[1]}return a[0].to_array().join(c)});define_libfunc("intersperse",2,2,function(c){var e=c[0],a=c[1];assert_list(a);var d=[];a.to_array().reverse().each(function(f){d.push(f);d.push(e)});d.pop();return d.to_list()});define_libfunc("map-with-index",2,null,function(d){var c=d.shift(),a=d;a.each(function(g){assert_list(g)});var f=[],e=0;return Call.multi_foreach(a,{call:function(h){var g=h.map(function(j){return j.car});g.unshift(e);e++;return new Call(c,g)},result:function(g){f.push(g)},finish:function(){return f.to_list()}})});var sort_with_comp=function(c,a){return c.sort(function(e,d){var f=new BiwaScheme.Interpreter();return f.invoke_closure(a,[e,d])})};define_libfunc("list-sort/comp",1,2,function(a){assert_procedure(a[0]);assert_list(a[1]);return sort_with_comp(a[1].to_array(),a[0]).to_list()});define_libfunc("vector-sort/comp",1,2,function(a){assert_procedure(a[0]);assert_vector(a[1]);return sort_with_comp(a[1].clone(),a[0])});define_libfunc("vector-sort/comp!",1,2,function(a){assert_procedure(a[0]);assert_vector(a[1]);sort_with_comp(a[1],a[0]);return BiwaScheme.undef});var rearrange_args=function(d,f){var a=[];var e=(new Compiler).find_dot_pos(d);if(e==-1){a=f}else{for(var c=0;c<e;c++){a[c]=f[c]}a[c]=f.slice(c).to_list()}return a};define_syntax("define-macro",function(c){var h=c.cdr.car;var e;if(h instanceof Pair){var g=h.car;e=h.cdr;var a=c.cdr.cdr;var f=new Pair(Sym("lambda"),new Pair(e,a))}else{var g=h;var f=c.cdr.cdr.car;e=f.cdr.car}var j=Compiler.compile(f);if(j[1]!=0){throw new Bug("you cannot use free variables in macro expander (or define-macro must be on toplevel)")}var d=[j[2]];TopEnv[g.name]=new Syntax(g.name,function(q){var o=q.to_array();o.shift();var l=new Interpreter();var m=rearrange_args(e,o);var k=l.invoke_closure(d,m);return k});return BiwaScheme.undef});var macroexpand_1=function(c){if(c instanceof Pair){if(c.car instanceof Symbol&&TopEnv[c.car.name] instanceof Syntax){var a=TopEnv[c.car.name];c=a.transform(c)}else{throw new Error("macroexpand-1: `"+to_write_ss(c)+"' is not a macro")}}return c};define_syntax("%macroexpand",function(a){var c=(new Interpreter).expand(a.cdr.car);return[Sym("quote"),c].to_list()});define_syntax("%macroexpand-1",function(a){var c=macroexpand_1(a.cdr.car);return[Sym("quote"),c].to_list()});define_libfunc("macroexpand",1,1,function(a){return(new Interpreter).expand(a[0])});define_libfunc("macroexpand-1",1,1,function(a){return macroexpand_1(a[0])});define_libfunc("gensym",0,0,function(a){return BiwaScheme.gensym()});define_libfunc("print",1,null,function(a){a.map(function(c){puts(to_display(c),true)});puts("");return BiwaScheme.undef});define_libfunc("write-to-string",1,1,function(a){return to_write(a[0])});define_libfunc("read-from-string",1,1,function(a){assert_string(a[0]);return Interpreter.read(a[0])});define_libfunc("port-closed?",1,1,function(a){assert_port(a[0]);return !(a[0].is_open)});define_syntax("let1",function(c){var d=c.cdr.car;var e=c.cdr.cdr.car;var a=c.cdr.cdr.cdr;return new Pair(new Pair(Sym("lambda"),new Pair(new Pair(d,nil),a)),new Pair(e,nil))});var assert_regexp=function(a,c){if(!(a instanceof RegExp)){throw new Error(c+": regexp required, but got "+to_write(a))}};define_libfunc("string->regexp",1,1,function(a){assert_string(a[0],"string->regexp");return new RegExp(a[0])});define_libfunc("regexp?",1,1,function(a){return(a[0] instanceof RegExp)});define_libfunc("regexp->string",1,1,function(a){assert_regexp(a[0],"regexp->string");return a[0].toString().slice(1,-1)});define_libfunc("regexp-exec",2,2,function(a){var d=a[0];if(Object.isString(a[0])){d=new RegExp(a[0])}assert_regexp(d,"regexp-exec");assert_string(a[1],"regexp-exec");var c=d.exec(a[1]);return(c===null)?false:c.to_list()})}with(BiwaScheme){define_libfunc("iota",1,3,function(d){var g=d[0];var j=d[1]||0;var f=(d[2]===undefined)?1:d[2];assert_integer(g);assert_number(j);assert_number(f);var c=[],h=j;for(var e=0;e<g;e++){c.push(h);h+=f}return c.to_list()});define_libfunc("open-input-string",1,1,function(a){assert_string(a[0]);return new Port.StringInput(a[0])});define_libfunc("open-output-string",0,0,function(a){return new Port.StringOutput()});define_libfunc("get-output-string",1,1,function(a){assert_port(a[0]);if(!(a[0] instanceof Port.StringOutput)){throw new Error("get-output-string: port must be made by 'open-output-string'")}return a[0].output_string()});define_libfunc("current-date",0,1,function(a){return new Date()});define_libfunc("date?",1,1,function(a){return(a[0] instanceof Date)});define_libfunc("date-nanosecond",1,1,function(a){assert_date(a[0]);return a[0].getMilliseconds()*1000000});define_libfunc("date-millisecond",1,1,function(a){assert_date(a[0]);return a[0].getMilliseconds()});define_libfunc("date-second",1,1,function(a){assert_date(a[0]);return a[0].getSeconds()});define_libfunc("date-minute",1,1,function(a){assert_date(a[0]);return a[0].getMinutes()});define_libfunc("date-hour",1,1,function(a){assert_date(a[0]);return a[0].getHours()});define_libfunc("date-day",1,1,function(a){assert_date(a[0]);return a[0].getDate()});define_libfunc("date-month",1,1,function(a){assert_date(a[0]);return a[0].getMonth()+1});define_libfunc("date-year",1,1,function(a){assert_date(a[0]);return a[0].getFullYear()});define_libfunc("date-week-day",1,1,function(a){assert_date(a[0]);return a[0].getDay()});BiwaScheme.date_names={weekday:["Sun","Mon","Tue","Wed","Thu","Fri","Sat"],full_weekday:["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"],month:["Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"],full_month:["January","February","March","April","May","June","July","August","September","Octorber","November","December"]};BiwaScheme.date2string=function(d,e){var f=function(g){return g<10?"0"+g:""+g};var c=function(g){return g<10?" "+g:""+g};var a={a:function(g){return date_names.weekday[g.getDay()]},A:function(g){return date_names.full_weekday[g.getDay()]},b:function(g){return date_names.month[g.getMonth()]},B:function(g){return date_names.full_month[g.getMonth()]},c:function(g){return g.toString()},d:function(g){return f(g.getDate())},D:function(g){return a.d(g)+a.m(g)+a.y(g)},e:function(g){return c(g.getDate())},f:function(g){return g.getSeconds()+g.getMilliseconds()/1000},h:function(g){return date_names.month[g.getMonth()]},H:function(g){return f(g.getHours())},I:function(g){var j=g.getHours();return f(j<13?j:j-12)},j:function(g){throw new Bug("not implemented: day of year")},k:function(g){return c(g.getHours())},l:function(g){var j=g.getHours();return c(j<13?j:j-12)},m:function(g){return f(g.getMonth())},M:function(g){return f(g.getMinutes())},n:function(g){return"\n"},N:function(g){throw new Bug("not implemented: nanoseconds")},p:function(g){return g.getHours()<13?"AM":"PM"},r:function(g){return a.I(g)+":"+a.M(g)+":"+a.S(g)+" "+a.p(g)},s:function(g){return Math.floor(g.getTime()/1000)},S:function(g){return f(g.getSeconds())},t:function(g){return"\t"},T:function(g){return a.H(g)+":"+a.M(g)+":"+a.S(g)},U:function(g){throw new Bug("not implemented: weeknum(0~, Sun)")},V:function(g){throw new Bug("not implemented: weeknum(1~, Sun?)")},w:function(g){return g.getDay()},W:function(g){throw new Bug("not implemented: weeknum(0~, Mon)")},x:function(g){throw new Bug("not implemented: weeknum(1~, Mon)")},X:function(g){return a.Y(g)+"/"+a.m(g)+"/"+a.d(g)},y:function(g){return g.getFullYear()%100},Y:function(g){return g.getFullYear()},z:function(g){throw new Bug("not implemented: time-zone")},Z:function(g){throw new Bug("not implemented: symbol time zone")},1:function(g){throw new Bug("not implemented: ISO-8601 year-month-day format")},2:function(g){throw new Bug("not implemented: ISO-8601 hour-minute-second-timezone format")},3:function(g){throw new Bug("not implemented: ISO-8601 hour-minute-second format")},4:function(g){throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second-timezone format")},5:function(g){throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second format")}};return e.replace(/~([\w1-5~])/g,function(h,g){var j=a[g];if(j){return j(d)}else{if(g=="~"){return"~"}else{return g}}})};define_libfunc("date->string",1,2,function(a){assert_date(a[0]);if(a[1]){assert_string(a[1]);return date2string(a[0],a[1])}else{return a[0].toString()}});define_libfunc("parse-date",1,1,function(a){assert_string(a[0]);return new Date(Date.parse(a[0]))});define_libfunc("random-integer",1,1,function(a){var c=a[0];assert_integer(c);if(c<0){throw new Error("random-integer: the argument must be >= 0")}else{return Math.floor(Math.random()*a[0])}});define_libfunc("random-real",0,0,function(a){return Math.random()});var user_write_ss=function(a){puts(write_ss(a[0]),true);return BiwaScheme.undef};define_libfunc("write/ss",1,2,user_write_ss);define_libfunc("write-with-shared-structure",1,2,user_write_ss);define_libfunc("write*",1,2,user_write_ss);define_libfunc("vector-append",2,null,function(a){var c=[];return c.concat.apply(c,a)})}with(BiwaScheme){BiwaScheme.Dumper=Class.create({initialize:function(a){this.dumparea=a||$("dumparea")||null;this.reset()},reset:function(){if(this.dumparea){$(this.dumparea).update("")}this.n_folds=0;this.closures=[];this.n_dumps=0;this.cur=-1;this.is_folded=true},is_opc:function(a){return(a instanceof Array&&typeof(a[0])=="string")},dump_pad:"&nbsp;&nbsp;&nbsp;",dump_opc:function(e,g){var c="";var f="",d="";var g=g||0;g.times(function(){f+=this.dump_pad}.bind(this));(g+1).times(function(){d+=this.dump_pad}.bind(this));c+=f+'[<span class="dump_opecode">'+e[0]+"</span>";var a=1;while(!(e[a] instanceof Array)&&a<e.length){if(e[0]=="constant"){c+="&nbsp;<span class='dump_constant'>"+this.dump_obj(e[a])+"</span>"}else{c+="&nbsp;"+this.dump_obj(e[a])}a++}if(a<e.length){c+="<br>\n"}for(;a<e.length;a++){if(this.is_opc(e[a])){c+=this.dump_opc(e[a],(a==e.length-1?g:g+1))}else{c+=(a==e.length-1)?f:d;c+=this.dump_obj(e[a])}if(a!=e.length-1){c+="<br>\n"}}c+="]";return(g==0?this.add_fold(c):c)},fold_limit:20,add_fold:function(f){var c=f.split(/<br>/gmi);if(c.length>this.fold_limit){var e=" <span style='text-decoration:underline; color:blue; cursor:pointer;'onclick='BiwaScheme.Dumper.toggle_fold("+this.n_folds+")'>more</span>";var a="<div style='display:none' id='fold"+this.n_folds+"'>";var d="</div>";this.n_folds++;return[c.slice(0,this.fold_limit).join("<br>"),e,a,c.slice(this.fold_limit+1).join("<br>"),d].join("")}else{return f}},stack_max_len:80,dump_stack:function(f,d){if(f===null||f===undefined){return Object.inspect(f)}var e="<table>";if(f.length==0){e+="<tr><td class='dump_dead'>(stack is empty)</td></tr>"}else{if(d<f.length){var a=f.length-1;e+="<tr><td class='dump_dead'>["+a+"]</td><td class='dump_dead'>"+this.dump_obj(f[a]).truncate(this.stack_max_len)+"</td></tr>"}}for(var c=d-1;c>=0;c--){e+="<tr><td class='dump_stknum'>["+c+"]</td><td>"+this.dump_obj(f[c]).truncate(this.stack_max_len)+"</td></tr>"}return e+"</table>"},dump_object:function(e){var c=[];for(var d in e){c.push(d.toString())}return"#<Object{"+c.join(",")+"}>"},dump_closure:function(d){if(d.length==0){return"[]"}var g=null;for(var e=0;e<this.closures.length;e++){if(this.closures[e]==d){g=e}}if(g==null){g=this.closures.length;this.closures.push(d)}var f=d.clone?d.clone():[f];var a=f.shift();return["c",g," <span class='dump_closure'>free vars :</span> ",this.dump_obj(f)," <span class='dump_closure'>body :</span> ",this.dump_obj(a).truncate(100)].join("")},dump_obj:function(c){if(c&&typeof(c.to_html)=="function"){return c.to_html()}else{var a=write_ss(c,true);if(a=="[object Object]"){a=this.dump_object(c)}return a.escapeHTML()}},dump:function(d){var c=document.createElement("div");var a="";if(d instanceof Hash){a+="<table>";a+="<tr><td colspan='4'><a href='#' id='dump_"+this.n_dumps+"_header'>#"+this.n_dumps+"</a></td></tr>";d.each(function(f){if(f.key!="x"&&f.key!="stack"){var e=(f.key=="c"?this.dump_closure(f.value):this.dump_obj(f.value));a+="<tr><td>"+f.key+": </td><td colspan='3'>"+e+"</td></tr>"}}.bind(this));a+="<tr><td>x:</td><td>"+(this.is_opc(d.get("x"))?this.dump_opc(d.get("x")):this.dump_obj(d.get("x")))+"</td>";a+="<td style='border-left: 1px solid black'>stack:</td><td>"+this.dump_stack(d.get("stack"),d.get("s"))+"</td></tr>";a+="</table>"}else{a=Object.inspect(d).escapeHTML()+"<br>\n"}c.id="dump"+this.n_dumps;c.innerHTML=a;this.dumparea.appendChild(c);(function(e){$("dump_"+this.n_dumps+"_header").observe("click",function(){this.dump_move_to(e);this.dump_fold()}.bind(this))}.bind(this))(this.n_dumps);Element.hide(c);this.n_dumps++},dump_move_to:function(a){if(0<=a&&a<=this.n_dumps){Element.hide($("dump"+this.cur));this.cur=a;Element.show($("dump"+this.cur))}},dump_move:function(a){if(0<=this.cur&&this.cur<this.n_dumps){Element.hide($("dump"+this.cur))}if(0<=this.cur+a&&this.cur+a<this.n_dumps){this.cur+=a}Element.show($("dump"+this.cur))},dump_fold:function(){for(var a=0;a<this.n_dumps;a++){if(a!=this.cur){Element.hide($("dump"+a))}}this.is_folded=true},dump_unfold:function(){for(var a=0;a<this.n_dumps;a++){Element.show($("dump"+a))}this.is_folded=false},dump_toggle_fold:function(){if(this.is_folded){this.dump_unfold()}else{this.dump_fold()}}})}BiwaScheme.Dumper.toggle_fold=function(a){Element.toggle("fold"+a)};
=======
var BiwaScheme = BiwaScheme || {};

BiwaScheme.Version = "0.5.7";
BiwaScheme.GitCommit = "e1d108943e475c8e9c1d6107c92f2379ba5e6393";
//
// Super-simple class implementation
//
// Example usage:
//
// BiwaScheme.Foo = BiwaScheme.Class.create({
//   initialize: function(a){
//     this.a = a;
//   },
//
//   toString: function(){
//     return "foo[" + this.a + "]";
//   }
// });
//
// BiwaScheme.Bar = BiwaScheme.Class.extend(new BiwaScheme.Foo("hello1"), {
//   initialize: function(b){
//     this.b = b;
//   },
//
//   printEverything: function(){
//     console.log("a = ", this.a, "b = ", this.b);
//   },
//
//   toString: function(){
//     return "bar[" + this.b + "]";
//   }
// });

BiwaScheme.Class = {
  create: function(methods) {
    var klass = function(){ this.initialize.apply(this, arguments); };
    _.extend(klass.prototype, methods);
    return klass;
  },

  extend: function(parent, methods) {
    var klass = function(){ this.initialize.apply(this, arguments); };
    klass.prototype = parent;
    _.extend(klass.prototype, methods);
    return klass;
  }
};
// 
// Heap based scheme from 3imp.pdf
//

// default definition of puts: should be overriden for console interpreters

if (typeof(Console) === 'undefined') {

}

function puts(str, no_newline){
    Console.puts(str, no_newline)
}
function p(/*args*/){
    Console.p.apply(this, arguments)
}

//
// variables
//
BiwaScheme.TopEnv = {};
BiwaScheme.CoreEnv = {};

//
// Errors (temporary?)
//

BiwaScheme.Error = BiwaScheme.Class.create({
  initialize: function(msg){
    this.message = "Error: "+msg;
  },
  toString: function(){
    return this.message;
  }
});

BiwaScheme.Bug = BiwaScheme.Class.extend(new BiwaScheme.Error(), {
  initialize: function(msg){
    this.message = "[BUG] "+msg;
  }
});

// currently used by "raise"
BiwaScheme.UserError = BiwaScheme.Class.extend(new BiwaScheme.Error(), {
  initialize: function(msg){
    this.message = msg;
  }
});

BiwaScheme.inspect = function(object) {
  try {
    if (_.isUndefined(object)) return 'undefined';
    if (object === null) return 'null';
    if (object.inspect) return object.inspect();
    if (_.isString(object)) {
      return "'" + object.replace(/'/g, '\\\'') + "'";
    }
    if (_.isArray(object)) {
      return '[' + _.map(object, BiwaScheme.inspect).join(', ') + ']';
    }
    return object.toString();
  } catch (e) {
    if (e instanceof RangeError) return '...';
    throw e;
  }
};
//
// Set - set of string
// contents must be string (or at least sortable)
//
BiwaScheme.Set = BiwaScheme.Class.create({
  initialize : function(/*args*/){
    this.arr = [];
    var i;
    for(i=0; i<arguments.length; i++)
      this.arr[i] = arguments[i];
  },

  equals : function(other){
    if(this.arr.length != other.arr.length)
      return false;

    var a1 = _.clone(this.arr);
    var a2 = _.clone(other.arr);
    a1.sort();
    a2.sort();
    for(var i=0; i<this.arr.length; i++){
      if(a1[i] != a2[i]) return false;
    }
    return true;
  },
  set_cons : function(item){
    var o = new BiwaScheme.Set(item);
    o.arr = _.clone(this.arr);
    o.arr.push(item);
    return o;
  },
  set_union : function(/*args*/){
    var o = new BiwaScheme.Set();
    o.arr = _.clone(this.arr);

    for(var k=0; k<arguments.length; k++){
      var s2 = arguments[k];
      if(!s2 instanceof BiwaScheme.Set)
        throw new BiwaScheme.Error("set_union: arguments must be a set");

      for(var i=0; i<s2.arr.length; i++)
        o.add(s2.arr[i]);
    }
    return o;
  },
  set_intersect : function(s2){
    if(!s2 instanceof BiwaScheme.Set)
      throw new BiwaScheme.Error("set_intersect: arguments must be a set");

    var o = new BiwaScheme.Set();
    for(var i=0; i<this.arr.length; i++)
      if(s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  },
  set_minus : function(s2){
    if(!s2 instanceof BiwaScheme.Set)
      throw new BiwaScheme.Error("set_minus: arguments must be a set");

    var o = new BiwaScheme.Set();
    for(var i=0; i<this.arr.length; i++)
      if(!s2.member(this.arr[i]))
        o.add(this.arr[i]);
    return o;
  },
  add : function(item){
    if(!this.member(item)){
      this.arr.push(item);
    }
  },
  member : function(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return true;

    return false;
  },
  rindex : function(item){
    for(var i=this.arr.length-1; i>=0 ; i--)
      if(this.arr[i] == item) return (this.arr.length-1-i);

    return null;
  },
  index : function(item){
    for(var i=0; i<this.arr.length; i++)
      if(this.arr[i] == item) return i;

    return null;
  },
  inspect : function(){
    return "Set(" + this.arr.join(", ") + ")";
  },
  toString : function(){
    return this.inspect();
  },
  size : function(){
    return this.arr.length;
  }
});
//
// utility functions
//
BiwaScheme.to_write = function(obj){
  if(obj === undefined)
    return "undefined";
  else if(obj === null)
    return "null";
  else if(_.isFunction(obj))
    return "#<Function "+(obj.fname ? obj.fname :
                          obj.toSource ? _.truncate(obj.toSource(), 40) :
                          "")+">";
  else if(_.isString(obj))
    return '"' +
           obj.replace(/\\|\"/g,function($0){return'\\'+$0;})
              .replace(/\x07/g, "\\a")
              .replace(/\x08/g, "\\b")
              .replace(/\t/g, "\\t")
              .replace(/\n/g, "\\n")
              .replace(/\v/g, "\\v")
              .replace(/\f/g, "\\f")
              .replace(/\r/g, "\\r") +
           '"';
  else if(_.isArray(obj) && obj.closure_p)
    return "#<Closure>";
  else if(_.isArray(obj))
    return "#(" + _.map(obj, function(e) { return BiwaScheme.to_write(e); }).join(" ") + ")";
  else if(typeof(obj.to_write) == 'function')
    return obj.to_write();
  else if(isNaN(obj) && typeof(obj) == 'number')
    return "+nan.0";
  else{
    switch(obj){
      case true: return "#t";
      case false: return "#f";
      case Infinity: return "+inf.0";
      case -Infinity: return "-inf.0";
    }
  }
  return BiwaScheme.inspect(obj);
}
BiwaScheme.to_display = function(obj){
  if(typeof(obj.valueOf()) == "string")
    return obj;
  else if(obj instanceof BiwaScheme.Symbol)
    return obj.name;
  else if(obj instanceof Array)
    return '#(' + _.map(obj, BiwaScheme.to_display).join(' ') + ')';
  else if(obj instanceof BiwaScheme.Pair)
    return obj.inspect(BiwaScheme.to_display);
  else if(obj instanceof BiwaScheme.Char)
    return obj.value;
  else
    return BiwaScheme.to_write(obj);
}

// write/ss (write with substructure)
// example:  > (let ((x (list 'a))) (list x x))                   //           (#0=(a) #0#)
// 2-pass algorithm.
// (1) detect all the objects which appears more than once
//     (find_cyclic, reduce_cyclic_info)
// (2) write object using this information
//   * add prefix '#n=' for first appearance
//   * just write '#n#' for other appearance

//TODO: support Values
BiwaScheme.write_ss = function(obj, array_mode){
  var known = [obj], used = [false];
  BiwaScheme.find_cyclic(obj, known, used);
  var cyclic   = BiwaScheme.reduce_cyclic_info(known, used);
  var appeared = new Array(cyclic.length);
  for(var i=cyclic.length-1; i>=0; i--) appeared[i] = false;

  return BiwaScheme.to_write_ss(obj, cyclic, appeared, array_mode);
}
BiwaScheme.to_write_ss = function(obj, cyclic, appeared, array_mode){
  var ret = "";
  var i = cyclic.indexOf(obj);
  if(i >= 0){
    if(appeared[i]){
      return "#"+i+"#";
    }
    else{
      appeared[i] = true;
      ret = "#"+i+"=";
    }
  }

  if(obj instanceof BiwaScheme.Pair){
    var a = [];
    a.push(BiwaScheme.to_write_ss(obj.car, cyclic, appeared, array_mode));
    for(var o=obj.cdr; o != BiwaScheme.nil; o=o.cdr){
      if(!(o instanceof BiwaScheme.Pair) || cyclic.indexOf(o) >= 0){
        a.push(".");
        a.push(BiwaScheme.to_write_ss(o, cyclic, appeared, array_mode));
        break;
      }
      a.push(BiwaScheme.to_write_ss(o.car, cyclic, appeared, array_mode));
    }
    ret += "(" + a.join(" ") + ")";
  }
  else if(obj instanceof Array){
    var a = _.map(obj, function(item){
      return BiwaScheme.to_write_ss(item, cyclic, appeared, array_mode);
    })
    if(array_mode)
      ret += "[" + a.join(", ") + "]";
    else
      ret += "#(" + a.join(" ") + ")";
  }
  else{
    ret += BiwaScheme.to_write(obj);
  }
  return ret;
}
BiwaScheme.reduce_cyclic_info = function(known, used){
  var n_used = 0;
  for(var i=0; i<used.length; i++){
    if(used[i]){
      known[n_used] = known[i];
      n_used++;
    }
  }
  return known.slice(0, n_used);
}
BiwaScheme.find_cyclic = function(obj, known, used){
  var items = (obj instanceof BiwaScheme.Pair)  ? [obj.car, obj.cdr] :
              (obj instanceof Array) ? obj :
              null;
  if(!items) return;

  _.each(items, function(item){
    if(typeof(item)=='number' || typeof(item)=='string' ||
      item === BiwaScheme.undef || item === true || item === false ||
      item === BiwaScheme.nil || item instanceof BiwaScheme.Symbol) return;

    var i = known.indexOf(item);
    if(i >= 0)
      used[i] = true;
    else{
      known.push(item);
      used.push(false);
      BiwaScheme.find_cyclic(item, known, used);
    }
  });
};


//
// Pair 
// cons cell
//

BiwaScheme.Pair = BiwaScheme.Class.create({
  initialize: function(car, cdr){
    this.car = car;
    this.cdr = cdr;
  },

  caar: function(){ return this.car.car; },
  cadr: function(){ return this.cdr.car; },
  cdar: function(){ return this.cdr.car; },
  cddr: function(){ return this.cdr.cdr; },

  first:  function(){ return this.car; },
  second: function(){ return this.cdr.car; },
  third:  function(){ return this.cdr.cdr.car; },
  fourth: function(){ return this.cdr.cdr.cdr.car; },
  fifth:  function(){ return this.cdr.cdr.cdr.cdr.car; },

  // returns array containing all the car's of list
  // '(1 2 3) => [1,2,3]
  // '(1 2 . 3) => [1,2]
  to_array: function(){
    var ary = [];
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      ary.push(o.car);
    }
    return ary;
  },

  to_set: function(){
    var set = new BiwaScheme.Set();
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      set.add(o.car);
    }
    return set;
  },

  length: function(){
    var n = 0;
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      n++;
    }
    return n;
  },

  // calls the given func passing each car of list
  // returns cdr of last Pair
  foreach: function(func){
    for(var o = this; o instanceof BiwaScheme.Pair; o=o.cdr){
      func(o.car);
    }
    return o;
  },

  // Returns an array which contains the resuls of calling func
  // with the car's as an argument.
  // If the receiver is not a proper list, the last cdr is ignored.
  // The receiver must not be a cyclic list.
  map: function(func){
    var ary = [];
    for(var o = this; BiwaScheme.isPair(o); o = o.cdr){
      ary.push(func(o.car));
    }
    return ary;
  },

  // Destructively concat the given list to the receiver.
  // The receiver must be a proper list.
  // Returns the receiver.
  concat: function(list){
    var o = this;
    while(o instanceof BiwaScheme.Pair && o.cdr != BiwaScheme.nil){
      o = o.cdr;
    }
    o.cdr = list;
    return this;
  },

  // returns human-redable string of pair
  inspect: function(conv){
    conv || (conv = BiwaScheme.inspect);
    var a = [];
    var last = this.foreach(function(o){
      a.push(conv(o));
    });
    if(last != BiwaScheme.nil){
      a.push(".");
      a.push(conv(last));
    }
    return "(" + a.join(" ") + ")";
  },
  toString : function(){
    return this.inspect();
  },

  to_write: function(){
    return this.inspect(BiwaScheme.to_write);
  }
});

// Creates a list out of the arguments, converting any nested arrays into nested lists.
// Example:
//   BiwaScheme.List(1, 2, [3, 4]) ;=> (1 2 (3 4))
var array_to_list = function(ary, deep) {
  var list = BiwaScheme.nil;
  for(var i=ary.length-1; i>=0; i--){
    var obj = ary[i];
    if(deep && _.isArray(obj) && !obj.is_vector){
      obj = BiwaScheme.array_to_list(obj);
    }
    list = new BiwaScheme.Pair(obj, list);
  }
  return list;
}
BiwaScheme.List = function() {
  var ary = _.toArray(arguments);
  return array_to_list(ary, true);
};
BiwaScheme.array_to_list = function(array) {
  return BiwaScheme.List.apply(null, array);
};
BiwaScheme.shallow_array_to_list = function(ary) {
  return array_to_list(ary, false);
};
//
// Values
//
BiwaScheme.Values = BiwaScheme.Class.create({
  initialize: function(values){
    this.content = values;
  },
  to_write: function(){
    return "#<Values " +
             _.map(this.content, BiwaScheme.to_write).join(" ") +
           ">";
  }
});

//
// Nil
// javascript representation of empty list( '() )
//
BiwaScheme.nil = {
  toString: function() { return "nil"; },
  to_write: function() { return "()"; },
  to_array: function() { return []; },
  length: function() { return 0; }
};

//
// #<undef> (The undefined value)
// also used as #<unspecified> values
//
BiwaScheme.undef = new Object();
BiwaScheme.undef.toString = function(){ return "#<undef>"; }

// (eof-object)
BiwaScheme.eof = new Object;
//
// Symbol
//

BiwaScheme.Symbol = BiwaScheme.Class.create({
  initialize: function(str){
    this.name = str;
    BiwaScheme.Symbols[ str ] = this;
  },

  inspect: function(){
    return "'"+this.name;
    //return "#<Symbol '"+this.name+"'>";
  },

  toString: function(){
    return "'"+this.name;
  },

  to_write: function(){
    return this.name;
  }
});
BiwaScheme.Symbols = {};
BiwaScheme.Sym = function(name,leaveCase){
  if( BiwaScheme.Symbols[name] === undefined ){
    return new BiwaScheme.Symbol(name);
  }
  else if( ! (BiwaScheme.Symbols[name] instanceof BiwaScheme.Symbol) ){ //pre-defined member (like 'eval' in Firefox)
    return new BiwaScheme.Symbol(name);
  }
  else{
    return BiwaScheme.Symbols[name];
  }
}

BiwaScheme.gensyms = 0;
BiwaScheme.gensym = function(){
  BiwaScheme.gensyms++;
  return BiwaScheme.Sym("__gensym_" + BiwaScheme.gensyms);
};
//
// Char
//

BiwaScheme.Char = BiwaScheme.Class.create({
  initialize: function(c){
    BiwaScheme.Chars[ this.value = c ] = this;
  },
  to_write: function(){
    switch(this.value){
      case '\n': return "#\\newline";
      case ' ':  return "#\\space";
      case '\t': return "#\\tab";
      default:   return "#\\"+this.value;
    }
  },
  inspect: function(){
    return this.to_write();
  }
});
BiwaScheme.Chars = {};
BiwaScheme.Char.get = function(c) {
  if(typeof(c) != "string") {
    throw new BiwaScheme.Bug("Char.get: " +
                             BiwaScheme.inspect(c) + " is not a string");
  }
  if( BiwaScheme.Chars[c] === undefined )
    return new BiwaScheme.Char(c);
  else
    return BiwaScheme.Chars[c];
};

//
// number.js - Complex and Rational
//

BiwaScheme.Complex = BiwaScheme.Class.create({
  initialize: function(real, imag){
    this.real = real;
    this.imag = imag;
  },
  magnitude: function(){
    return Math.sqrt(this.real * this.real + this.imag * this.imag);
  },
  angle: function(){
    return Math.acos(this.real / this.magnitude());
  }
})
BiwaScheme.Complex.from_polar = function(r, theta){
  var real = r * Math.cos(theta);
  var imag = r * Math.sin(theta);
  return new BiwaScheme.Complex(real, imag);
}
BiwaScheme.Complex.assure = function(num){
  if(num instanceof BiwaScheme.Complex)
    return num
  else
    return new BiwaScheme.Complex(num, 0);
}

BiwaScheme.Rational = BiwaScheme.Class.create({
  initialize: function(numerator, denominator){
    this.numerator = numerator;
    this.denominator = denominator;
  }
})


//
// Port
//
BiwaScheme.Port = BiwaScheme.Class.create({
  initialize: function(is_in, is_out){
    this.is_open = true;
    this.is_binary = false; //??
    this.is_input = is_in;
    this.is_output = is_out;
  },
  close: function(){
    // close port
    this.is_open = false;
  },
  inspect: function(){
    return "#<Port>";
  },
  to_write: function(){
    return "#<Port>";
  }
});
BiwaScheme.Port.BrowserInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(){
  },
  get_string: function(after){
    var form = $("<form/>");
    form.html("<input id='webscheme-read-line' type='text'><input type='submit' value='ok'>");
    $("#bs-console").append(form);

    return new BiwaScheme.Pause(function(pause){
      form.submit(function(){
        var input = $("#webscheme-read-line").val();
        form.remove();
        puts(input);
        pause.resume(after(input));
        return false;
      });
    });
  }
})
BiwaScheme.Port.DefaultOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(){
  },
  put_string: function(str){
    puts(str, true);
  }
})

//
// string ports (srfi-6)
//
BiwaScheme.Port.StringOutput = BiwaScheme.Class.extend(new BiwaScheme.Port(false, true), {
  initialize: function(){
    this.buffer = [];
  },
  put_string: function(str){
    this.buffer.push(str);
  },
  output_string: function(str){
    return this.buffer.join("");
  }
});
BiwaScheme.Port.StringInput = BiwaScheme.Class.extend(new BiwaScheme.Port(true, false), {
  initialize: function(str){
    this.str = str;
  },
  get_string: function(after){
    return after(this.str);
  }
});
BiwaScheme.Port.current_input  = new BiwaScheme.Port.BrowserInput();
BiwaScheme.Port.current_output = new BiwaScheme.Port.DefaultOutput();
BiwaScheme.Port.current_error  = new BiwaScheme.Port.DefaultOutput();
//
// Record
//
BiwaScheme.Record = BiwaScheme.Class.create({
  initialize: function(rtd, values){
    assert_record_td(rtd, "new Record");

    this.rtd = rtd;
    this.fields = values;
  },
  get: function(k){
    return this.fields[k]
  },
  set: function(k, v){
    this.fields[k] = v;
  },
  toString: function(){
    var contents = BiwaScheme.to_write(this.fields);
    return "#<Record "+this.rtd.name+" "+contents+">";
  }
});
BiwaScheme.isRecord = function(o){
  return (o instanceof BiwaScheme.Record);
};

// Record types
BiwaScheme.Record._DefinedTypes = {};

BiwaScheme.Record.define_type = function(name_str, rtd, cd){
  return BiwaScheme.Record._DefinedTypes[name_str] = {rtd: rtd, cd: cd};
};
BiwaScheme.Record.get_type = function(name_str){
  return BiwaScheme.Record._DefinedTypes[name_str];
};

// Record type descriptor
BiwaScheme.Record.RTD = BiwaScheme.Class.create({
  //                   Symbol RTD        Symbol Bool  Bool    Array
  initialize: function(name, parent_rtd, uid, sealed, opaque, fields){
    this.name = name;
    this.parent_rtd = parent_rtd;
    this.is_base_type = !parent_rtd;
    if(uid){
      this.uid = uid;
      this.generative = false;
    }
    else{
      this.uid = this._generate_new_uid();;
      this.generative = true;
    }

    this.sealed = !!sealed;
    this.opaque = parent_rtd.opaque || (!!opaque);

    this.fields = _.map(fields, function(field){
      return {name: field[0], mutable: !!field[1]};
    });
  },
  _generate_new_uid: function(){
    var n = (BiwaScheme.Record.RTD.last_uid++);
    return BiwaScheme.Sym("__record_td_uid_"+n);
  },
  toString: function(){
    return "#<RecordTD "+name+">";
  }
});
BiwaScheme.Record.RTD.last_uid = 0;
BiwaScheme.Record.RTD.NongenerativeRecords = {};
BiwaScheme.isRecordTD = function(o){
  return (o instanceof BiwaScheme.Record.RTD);
};

// Record constructor descriptor
BiwaScheme.Record.CD = BiwaScheme.Class.create({
  initialize: function(rtd, parent_cd, protocol){
    this._check(rtd, parent_cd, protocol);
    this.rtd = rtd;
    this.parent_cd = parent_cd;
    if(protocol){
      this.has_custom_protocol = true;
      this.protocol = protocol;
    }
    else{
      this.has_custom_protocol = false;
      if(rtd.parent_rtd)
        this.protocol = this._default_protocol_for_derived_types();
      else
        this.protocol = this._default_protocol_for_base_types();
    }
  },

  _check: function(rtd, parent_cd, protocol){
    if(rtd.is_base_type && parent_cd)
      throw new Error("Record.CD.new: cannot specify parent cd of a base type");

    if(parent_cd && rtd.parent_rtd && (parent_cd.rtd != rtd.parent_rtd))
      throw new Error("Record.CD.new: mismatched parents between rtd and parent_cd");

    if(rtd.parent_rtd && !parent_cd && protocol)
      throw new Error("Record.CD.new: protocol must be #f when parent_cd is not given");

    if(parent_cd && parent_cd.has_custom_protocol && !protocol)
      throw new Error("Record.CD.new: protocol must be specified when parent_cd has a custom protocol");
  },
  
  _default_protocol_for_base_types: function(){
    // (lambda (p) p)
    // called with `p' as an argument
    return function(ar){
      var p = ar[0];
      assert_procedure(p, "_default_protocol/base");
      return p;
    };
  },

  _default_protocol_for_derived_types: function(){
    // (lambda (n) 
    //   (lambda (a b x y s t)
    //     (let1 p (n a b x y) (p s t))))
    // called with `n' as an argument
    var rtd = this.rtd;
    return function(ar){
      var n = ar[0];
      assert_procedure(n, "_default_protocol/n");

      var ctor = function(args){
        var my_argc = rtd.fields.length;
        var ancestor_argc = args.length - my_argc;

        var ancestor_values = args.slice(0, ancestor_argc);
        var my_values       = args.slice(ancestor_argc);

        // (n a b x y) => p
        return new BiwaScheme.Call(n, ancestor_values, function(ar){
          var p = ar[0];
          assert_procedure(p, "_default_protocol/p");

          // (p s t) => record
          return new BiwaScheme.Call(p, my_values, function(ar){
            var record = ar[0];
            assert_record(record, "_default_protocol/result");

            return record;
          });
        });
      };
      return ctor;
    };
  },

  toString: function(){
    return "#<RecordCD "+this.rtd.name+">";
  },

  record_constructor: function(){
    var arg_for_protocol = (this.parent_cd ? this._make_n([], this.rtd)
                                           : this._make_p());
    arg_for_protocol = _.bind(arg_for_protocol, this);

    return new BiwaScheme.Call(this.protocol, [arg_for_protocol], function(ar){
      var ctor = ar[0];
      assert_procedure(ctor, "record_constructor");
      return ctor;
    });
  },

  // Create the function `p' which is given to the protocol.
  _make_p: function(){
    return function(values){
      return new BiwaScheme.Record(this.rtd, values);
      // TODO: check argc 
    };
  },

  // Create the function `n' which is given to the protocol.
  // When creating an instance of a derived type,
  // _make_n is called for each ancestor rtd's.
  _make_n: function(children_values, rtd){
    var parent_cd = this.parent_cd;

    if(parent_cd){
      // called from protocol (n)
      var n = function(args_for_n){

        // called from protocol (p)
        var p = function(args_for_p){
          var values = [].concat(args_for_p[0]).concat(children_values)
          var parent_n = parent_cd._make_n(values, rtd);

          return new BiwaScheme.Call(parent_cd.protocol, [parent_n], function(ar){
            var ctor = ar[0];
            assert_procedure(ctor, "_make_n");

            return new BiwaScheme.Call(ctor, args_for_n, function(ar){
              var record = ar[0];
              assert_record(record);
              return record;
            });
          });
        };
        return p;
      };
      return n;
    }
    else{
      var n = function(my_values){
        var values = my_values.concat(children_values);
        return new BiwaScheme.Record(rtd, values);
        // TODO: check argc 
      };
      return n;
    }
  }
});

BiwaScheme.isRecordCD = function(o){
  return (o instanceof BiwaScheme.Record.CD);
};
//
// Hashtable
//
// Based on the base JavaScript Object class, but
//  * Object takes only strings as keys
//  * R6RS hashtable needs its own hash function
// so some hacks are needed.

BiwaScheme.Hashtable = BiwaScheme.Class.create({
  initialize: function(_hash_proc, _equiv_proc, mutable){
    this.mutable = (mutable === undefined) ? true :
                   mutable ? true : false;

    this.hash_proc = _hash_proc;
    this.equiv_proc = _equiv_proc;

    // Hash (hashed) => (array of (key and value))
    this.pairs_of = {};
  },

  clear: function(){
    this.pairs_of = {};
  },

  candidate_pairs: function(hashed){
    return this.pairs_of[hashed];
  },

  add_pair: function(hashed, key, value){
    var pairs = this.pairs_of[hashed];

    if (pairs) {
      pairs.push([key, value]);
    }
    else {
      this.pairs_of[hashed] = [[key, value]];
    }
  },

  remove_pair: function(hashed, pair){
    var pairs = this.pairs_of[hashed];
    var i = pairs.indexOf(pair);
    if (i == -1){
      throw new BiwaScheme.Bug("Hashtable#remove_pair: pair not found!");
    }
    else {
      pairs.splice(i, 1); //remove 1 element from i-th index
    }
  },

  create_copy: function(mutable){
    var copy = new BiwaScheme.Hashtable(this.hash_proc, this.equiv_proc,
                                        mutable);
    // clone the pairs to copy
    _.each(_.keys(this.pairs_of), _.bind(function(hashed){
      var pairs = this.pairs_of[hashed];
      var cloned = _.map(pairs, function(pair){
        return _.clone(pair);
      });
      copy.pairs_of[hashed] = cloned;
    }, this));

    return copy;
  },

  size: function(){
    var n = 0;
    this._apply_pair(function(pair){
      n++;
    });
    return n;
  },

  keys: function(){
    return this._apply_pair(function(pair){
      return pair[0];
    });
  },

  values: function(){
    return this._apply_pair(function(pair){
      return pair[1];
    });
  },

  _apply_pair: function(func){
    var a = [];
    _.each(_.values(this.pairs_of), function(pairs){
      _.each(pairs, function(pair){
        a.push(func(pair));
      });
    });
    return a;
  },

  to_write: function(){
    return "#<Hashtable size=" + this.size() + ">";
  }
});

//
// Hash functions
//

BiwaScheme.Hashtable.equal_hash = function(ar){
  return BiwaScheme.to_write(ar[0]);
};
BiwaScheme.Hashtable.eq_hash = BiwaScheme.Hashtable.equal_hash;
BiwaScheme.Hashtable.eqv_hash = BiwaScheme.Hashtable.equal_hash;

BiwaScheme.Hashtable.string_hash = function(ar){
  return ar[0];
};

BiwaScheme.Hashtable.string_ci_hash = function(ar){
  return _.isString(ar[0]) ? ar[0].toLowerCase() : ar[0];
};

BiwaScheme.Hashtable.symbol_hash = function(ar){
  return (ar[0] instanceof BiwaScheme.Symbol) ? ar[0].name : ar[0];
};

//
// Equivalence functions
//

BiwaScheme.Hashtable.eq_equiv = function(ar){
  return BiwaScheme.eq(ar[0], ar[1]);
};

BiwaScheme.Hashtable.eqv_equiv = function(ar){
  return BiwaScheme.eqv(ar[0], ar[1]);
};
//
// Syntax
//
BiwaScheme.Syntax = BiwaScheme.Class.create({
  initialize: function(sname, func){
    this.sname = sname;
    this.func = func;
  },
  transform: function(x){
    if (!this.func){
      throw new BiwaScheme.Bug("sorry, syntax "+this.sname+
                               " is a pseudo syntax now");
    }
    return this.func(x);
  },
  inspect: function(){
    return "#<Syntax " + this.sname +">";
  }
})

// A built-in syntax did not have associated Syntax object.
// Following code installed dummy Syntax objects to built-in syntax.
BiwaScheme.TopEnv["define"] = new BiwaScheme.Syntax("define");
BiwaScheme.TopEnv["begin"]  = new BiwaScheme.Syntax("begin");
BiwaScheme.TopEnv["quote"]  = new BiwaScheme.Syntax("quote");
BiwaScheme.TopEnv["lambda"] = new BiwaScheme.Syntax("lambda");
BiwaScheme.TopEnv["if"]     = new BiwaScheme.Syntax("if");
BiwaScheme.TopEnv["set!"]   = new BiwaScheme.Syntax("set!");
//
// types.js - type predicators
//

BiwaScheme.isNil = function(obj){
  return (obj === BiwaScheme.nil);
};

BiwaScheme.isUndef = function(obj){
  return (obj === BiwaScheme.undef);
};

BiwaScheme.isChar = function(obj){
  return (obj instanceof BiwaScheme.Char);
};

BiwaScheme.isSymbol = function(obj){
  return (obj instanceof BiwaScheme.Symbol);
};

BiwaScheme.isPort = function(obj){
  return (obj instanceof BiwaScheme.Port);
};

// Note: '() is not a pair in scheme
BiwaScheme.isPair = function(obj){
  return (obj instanceof BiwaScheme.Pair);
};

// Note: isList returns true for '()
BiwaScheme.isList = function(obj){
    if(obj === BiwaScheme.nil) return true; // null base case
    if(!(obj instanceof BiwaScheme.Pair)) return false;
    return BiwaScheme.isList(obj.cdr);
  //TODO: should check if it is not cyclic..
};

BiwaScheme.isVector = function(obj){
  return (obj instanceof Array) && (obj.closure_p !== true);
};

BiwaScheme.isHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable);
};

BiwaScheme.isMutableHashtable = function(obj){
  return (obj instanceof BiwaScheme.Hashtable) && obj.mutable;
};

BiwaScheme.isClosure = function(obj){
  return (obj instanceof Array) && (obj.closure_p === true);
};

// procedure: Scheme closure or JavaScript function
// valid argument for anywhere function is expected
BiwaScheme.isProcedure = function(obj){
  return BiwaScheme.isClosure(obj) || _.isFunction(obj);
};
  //
  // Parser 
  // copied from jsScheme - should be rewrriten (support #0=, etc)
  //
  BiwaScheme.Parser = BiwaScheme.Class.create({
    initialize: function(txt){
      this.tokens = this.tokenize(txt);
      this.i = 0;
    },

    inspect: function(){
      return [
        "#<Parser:",
        this.i, "/", this.tokens.length, " ",
        BiwaScheme.inspect(this.tokens),
        ">"
      ].join("");
    },

    tokenize: function(txt) {
      var tokens = new Array(), oldTxt=null;
      var in_srfi_30_comment = 0;

      while( txt != "" && oldTxt != txt ) {
        oldTxt = txt;
        txt = txt.replace( /^\s*(;[^\r\n]*(\r|\n|$)|#;|#\||#\\[^\w]|#?(\(|\[|{)|\)|\]|}|\'|`|,@|,|\+inf\.0|-inf\.0|\+nan\.0|\"(\\(.|$)|[^\"\\])*(\"|$)|[^\s()\[\]{}]+)/,
        function($0,$1) {
          var t = $1;

          if (t == "#|") {
            in_srfi_30_comment++;
            return "";
          }
          else if (in_srfi_30_comment > 0) {
            if ( /(.*\|#)/.test(t) ) {
              in_srfi_30_comment--;
              if (in_srfi_30_comment < 0) {
                throw new BiwaScheme.Error("Found an extra comment terminator: `|#'")
              }
              // Push back the rest substring to input stream.
              return t.substring(RegExp.$1.length, t.length);
            }
            else {
              return "";
            }
          }
          else {
            if( t.charAt(0) != ';' ) tokens[tokens.length]=t;
            return "";
          }
        } );
      }
      return tokens;
    },

    sexpCommentMarker: new Object,
    getObject: function() {
      var r = this.getObject0();

      if (r != this.sexpCommentMarker)
        return r;

      r = this.getObject();
      if (r == BiwaScheme.Parser.EOS)
        throw new BiwaScheme.Error("Readable object not found after S exression comment");

      r = this.getObject();
      return r;
    },
    
    getList: function( close ) {
      var list = BiwaScheme.nil, prev = list;
      while( this.i < this.tokens.length ) {

        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in list)");

        if( this.tokens[ this.i ] == ')' || this.tokens[ this.i ] == ']' || this.tokens[ this.i ] == '}' ) {
          this.i++; break;
        }

        if( this.tokens[ this.i ] == '.' ) {
          this.i++;
          var o = this.getObject();
          if( o != BiwaScheme.Parser.EOS && list != BiwaScheme.nil ) {
            prev.cdr = o;
          }
        } else {
            var cur = new BiwaScheme.Pair( this.getObject(), BiwaScheme.nil);
            if( list == BiwaScheme.nil ) list = cur;
            else prev.cdr = cur;
            prev = cur;
        }
      }
      return list;
    },

    getVector: function( close ) {
      var arr = new Array();
      while( this.i < this.tokens.length ) {
        
        this.eatObjectsInSexpComment("Input stream terminated unexpectedly(in vector)");
        
        if( this.tokens[ this.i ] == ')' ||
        this.tokens[ this.i ] == ']' ||
        this.tokens[ this.i ] == '}' ) { this.i++; break; }
        arr[ arr.length ] = this.getObject();
      }
      return arr;
    },

    eatObjectsInSexpComment: function(err_msg) {
      while( this.tokens[ this.i ] == '#;' ) {
        this.i++;
        if ((this.getObject() == BiwaScheme.Parser.EOS) || (this.i >= this.tokens.length))
          throw new BiwaScheme.Error(err_msg);  
      }
    }, 

    getObject0: function() {
      if( this.i >= this.tokens.length )
        return BiwaScheme.Parser.EOS;

      var t = this.tokens[ this.i++ ];
      // if( t == ')' ) return null;

      if (t == '#;')
        return this.sexpCommentMarker;

      var s = t == "'"  ? 'quote' :
              t == "`"  ? 'quasiquote' :
              t == ","  ? 'unquote' :
              t == ",@" ? 'unquote-splicing' : false;

      if( s || t == '(' || t == '#(' || t == '[' || t == '#[' || t == '{' || t == '#{' ) {
        return s ? new BiwaScheme.Pair( BiwaScheme.Sym(s), new BiwaScheme.Pair( this.getObject(), BiwaScheme.nil ))
        : (t=='(' || t=='[' || t=='{') ? this.getList(t) : this.getVector(t);
      } 
      else {
        switch(t){
          case "+inf.0" : return Infinity;
          case "-inf.0" : return -Infinity;
          case "+nan.0" : return NaN;
        }

        var n;
        if( /^#x[0-9a-z]+$/i.test(t) ) {  // #x... Hex
          n = new Number('0x'+t.substring(2,t.length) );
        } 
        else if( /^#d[0-9\.]+$/i.test(t) ) {  // #d... Decimal
          n = new Number( t.substring(2,t.length) );
        } 
        else{
          n = new Number(t);  // use constrictor as parser
        }

        if( ! isNaN(n) ) {
          return n.valueOf();
        } else if( t == '#f' || t == '#F' ) {
          return false;
        } else if( t == '#t' || t == '#T' ) {
          return true;
        } else if( t.toLowerCase() == '#\\newline' ) {
          return BiwaScheme.Char.get('\n');
        } else if( t.toLowerCase() == '#\\space' ) {
          return BiwaScheme.Char.get(' ');
        } else if( t.toLowerCase() == '#\\tab' ) {
          return BiwaScheme.Char.get('\t');
        } else if( /^#\\.$/.test(t) ) {
          return BiwaScheme.Char.get( t.charAt(2) );
        } else if( /^\"(\\(.|$)|[^\"\\])*\"?$/.test(t) ) {
          return t.replace(/(\r?\n|\\n)/g, "\n").replace( /^\"|\\(.|$)|\"$/g, function($0,$1) {
            return $1 ? $1 : '';
          } );
        } else return BiwaScheme.Sym(t);  // 2Do: validate !!
      }
    }
  });
  // indicates end of source file
  BiwaScheme.Parser.EOS = new Object();
  

///
/// Compiler
///

BiwaScheme.Compiler = BiwaScheme.Class.create({
  initialize: function(){
  },

  is_tail: function(x){
    return (x[0] == "return");
  },

  //free: set
  //e: env(= [locals, frees])
  //next: opc
  //ret: opc["refer_*", n, ["argument", 
  //          ["refer_*", n, ... ["argument", next]
  collect_free: function(free, e, next){
    var vars = free;
    var opc = next;
    var arr = vars.arr;
    for(var i=0; i<arr.length; i++){
      opc = this.compile_refer(arr[i], e, ["argument", opc]);
    }
    //puts("collect_free "+free.inspect()+" / "+e.inspect()+" => "+opc.inspect());
    return opc;
  },

  //x: Symbol
  //e: env [set of locals, set of frees]
  //ret: opc
  compile_refer: function(x, e, next){
    return this.compile_lookup(x, e,
             function(n){ return ["refer-local", n, next] },
             function(n){ return ["refer-free",  n, next] },
             function(sym){ return ["refer-global", sym, next] });
  },

  compile_lookup: function(x, e, return_local, return_free, return_global){
    var locals = e[0], free = e[1];
    if((n = locals.index(x)) != null){
      //puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-local "+n);
      return return_local(n);
    }
    else if((n = free.index(x)) != null){
      //puts("compile_refer:"+x.inspect()+" in "+e.inspect()+" results refer-free "+n);
      return return_free(n);
    }
    else{
      var sym = x.name;
      return return_global(sym);
    }
    //throw new BiwaScheme.Error("undefined symbol `" + sym + "'");
  },

  //generate boxing code (intersection of sets & vars)
  //if no need of boxing, just returns next
  //  sets(Set): assigned variables 
  //  vars(List): used variables
  //  next(opc):
  //  ret(opc):
  make_boxes: function(sets, vars, next){
    var vars = vars;
    var n = 0;
    var a = [];
    while(vars instanceof BiwaScheme.Pair){
      if(sets.member(vars.car))
        a.push(n);
      n++;
      vars = vars.cdr;
    }
    var opc = next;
    for(var i=a.length-1; i>=0; i--)
      opc = ["box", a[i], opc];
    return opc;
  },

  // Enumerate variables which (could be assigned && included in v)
  // x: exp
  // v: set(vars)
  // ret: set
  find_sets: function(x, v){
    //puts("find_sets: " + to_write(x) + " " + to_write(v))
    var ret=null;
    if(x instanceof BiwaScheme.Symbol){
      ret = new BiwaScheme.Set();
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.first()){
      case BiwaScheme.Sym("define"):
        var exp=x.third();
        ret = this.find_sets(exp, v);
      case BiwaScheme.Sym("begin"):
        ret = this.find_sets(x.cdr, v); //(ignores improper list)
        break;
      case BiwaScheme.Sym("quote"):
        ret = new BiwaScheme.Set();
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.second(), body=x.cdr.cdr;
        if (vars instanceof BiwaScheme.Pair){ // (lambda (...) ...)
          ret = this.find_sets(body, v.set_minus(vars.to_set()));
        }
        else { // (lambda args ...)
          ret = this.find_sets(body, v.set_minus(new BiwaScheme.Set(vars)));
        }
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        ret = this.find_sets(testc, v).set_union(
                        this.find_sets(thenc, v),
                        this.find_sets(elsec, v));
        break;
      case BiwaScheme.Sym("set!"):
        var vari=x.second(), xx=x.third();
        if(v.member(vari))
          ret = this.find_sets(xx, v).set_cons(vari);
        else
          ret = this.find_sets(xx, v);
        break;
      case BiwaScheme.Sym("call/cc"):
        var exp=x.second();
        ret = this.find_sets(exp, v);
        break;
      default:
        var set = new BiwaScheme.Set();
        for(var p=x; p instanceof BiwaScheme.Pair; p=p.cdr){
          set = set.set_union(this.find_sets(p.car, v));
        }
        ret = set;
        break;
      }
    }
    else{
      ret = new BiwaScheme.Set();
    }

    if(ret == null)
      throw new BiwaScheme.Bug("find_sets() exited in unusual way");
    else
      return ret;
  },

  // find_free(): find free variables in x
  //              these variables are collected by collect_free().
  // x: expression 
  // b: set of local vars (= variables which are not free)
  // f: set of free var candidates 
  //    (local vars of outer lambdas)
  // ret: set of free vars
  find_free: function(x, b, f){
    var ret=null;
    if(x instanceof BiwaScheme.Symbol){
      if(f.member(x))
        ret = new BiwaScheme.Set(x);
      else
        ret = new BiwaScheme.Set();
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.first()){
      case BiwaScheme.Sym("define"):
        var exp=x.third();
        ret = this.find_free(exp, b, f);
        break;
      case BiwaScheme.Sym("begin"):
        ret = this.find_free(x.cdr, b, f); //(ignores improper list)
        break;
      case BiwaScheme.Sym("quote"):
        ret = new BiwaScheme.Set();
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.second(), body=x.cdr.cdr;
        if (vars instanceof BiwaScheme.Pair){ // (lambda (...) ...)
          ret = this.find_free(body, b.set_union(vars.to_set()), f);
        }
        else { // (lambda args ...)
          ret = this.find_free(body, b.set_cons(vars), f);
        }
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        ret = this.find_free(testc, b, f).set_union(
                        this.find_free(thenc, b, f),
                        this.find_free(elsec, b, f));
        break;
      case BiwaScheme.Sym("set!"):
        var vari=x.second(), exp=x.third();
        if(f.member(vari))
          ret = this.find_free(exp, b, f).set_cons(vari);
        else
          ret = this.find_free(exp, b, f)
        break;
      case BiwaScheme.Sym("call/cc"):
        var exp=x.second();
        ret = this.find_free(exp, b, f);
        break;
      default:
        var set = new BiwaScheme.Set();
        for(var p=x; p instanceof BiwaScheme.Pair; p=p.cdr){
          set = set.set_union(this.find_free(p.car, b, f));
        }
        ret = set;
        break;
      }
    }
    else{
      ret = new BiwaScheme.Set();
    }
    //p("find_free "+x.inspect()+" / "+b.inspect()+" => "+ret.inspect());

    if(ret == null)
      throw new BiwaScheme.Bug("find_free() exited in unusual way");
    else
      return ret;
  },

  find_dot_pos: function(x){
    var idx = 0;
    for (; x instanceof BiwaScheme.Pair; x = x.cdr, ++idx)
      ;
    if (x != BiwaScheme.nil) {
      return idx;
    } else {
      return -1;
    }
  },

  last_pair: function(x){
    if (x instanceof BiwaScheme.Pair){
      for (; x.cdr instanceof BiwaScheme.Pair; x = x.cdr)
        ;
    }
    return x;
  },

  // dotted list -> proper list
  dotted2proper: function(ls){
    var nreverse = function(ls){
      var res = BiwaScheme.nil;
      for (; ls instanceof BiwaScheme.Pair; ){
        var d = ls.cdr;
        ls.cdr = res;
        res = ls;
        ls = d;
      }
      return res;
    }
    var copy_list = function(ls){
      var res = BiwaScheme.nil;
      for (; ls instanceof BiwaScheme.Pair; ls = ls.cdr){
        res = new BiwaScheme.Pair(ls.car, res);
      }
      return nreverse(res);
    }

    if (ls instanceof BiwaScheme.Pair) {
      var last = this.last_pair(ls);
      if (last instanceof BiwaScheme.Pair && last.cdr === BiwaScheme.nil){
        return ls;
      } else {
        var copied = copy_list(ls);
        this.last_pair(copied).cdr = new BiwaScheme.Pair(last.cdr, BiwaScheme.nil);
        return copied;
      }
    } else {
      return new BiwaScheme.Pair(ls, BiwaScheme.nil);
    }
  },

  // x: exp(list of symbol or integer or..)
  // e: env (= [locals, frees])
  // s: vars might be set!
  // next: opc
  // ret: opc
  compile: function(x, e, s, f, next){
    //p(x);
    var ret = null;

    while(1){
      if(x instanceof BiwaScheme.Symbol){
        return this.compile_refer(x, e, (s.member(x) ? ["indirect", next] : next));
      }
      else if(x instanceof BiwaScheme.Pair){
        switch(x.first()){
        case BiwaScheme.Sym("define"):
          // (define) ; => error
          // (define x 1 2) ; => error
          // x = (define <variable> <expression>)        : defines a variable
          // x = (define <variable>)                     : defines a variable with unspecified value
          // x = (define (<variable> <formals>) <body>)  : defines a function
          // x = (define (<variable> . <formal>) <body>) : defines a function with arbitary numbers of args

          if(x.length() == 1) { // i.e. (define)
            throw new BiwaScheme.Error("Invalid `define': "+x.to_write());
          }

          var left = x.cdr.car;
          var exp  = x.cdr.cdr;
          
          //define variable
          if(left instanceof BiwaScheme.Symbol){    
            if (exp === BiwaScheme.nil) {
              // eg. (define a)
              x = BiwaScheme.undef;
            }
            else {
              if (exp.cdr !== BiwaScheme.nil) {
                // eg. (define a 1 2)
                throw new BiwaScheme.Error("Invalid `define': "+x.to_write());
              }
              // eg. (define a 1)
              x = exp.car;
            }

            BiwaScheme.TopEnv[left.name] = BiwaScheme.undef;
            next = ["assign-global", left.name, next]; //should raise for improper list?
          }
          //define function 
          else if(left instanceof BiwaScheme.Pair){ 
            var fname=left.car, args=left.cdr;
            var lambda = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"), new BiwaScheme.Pair(args, exp));
            x = lambda;
            BiwaScheme.TopEnv[fname.name] = BiwaScheme.undef;
            next = ["assign-global", fname.name, next];
          }
          //error
          else{                          
            throw new BiwaScheme.Error("compile: define needs a leftbol or pair: got "+left);
          }
          break;

        case BiwaScheme.Sym("begin"):
          var a = [];
          for(var p=x.cdr; p instanceof BiwaScheme.Pair; p=p.cdr)
            a.push(p.car);

          //compile each expression (in reverse order)
          var c = next;
          for(var i=a.length-1; i>=0; i--){
            c = this.compile(a[i], e, s, f, c);
          }
          return c;

        case BiwaScheme.Sym("quote"):
          if(x.length() < 2)
              throw new BiwaScheme.Error("Invalid quote: "+x.to_write());

          var obj=x.second();
          return ["constant", obj, next];

        case BiwaScheme.Sym("lambda"):
          // x = '(lambda (x y) x y)
          // x = '(lambda vars x y)
          if(x.length() < 3)
              throw new BiwaScheme.Error("Invalid lambda: "+x.to_write());

          var vars = x.cdr.car;
          var body = new BiwaScheme.Pair(BiwaScheme.Sym("begin"), x.cdr.cdr); //tenuki

          var dotpos = this.find_dot_pos(vars);
          var proper = this.dotted2proper(vars);
          var free = this.find_free(body, proper.to_set(), f); //free variables
          var sets = this.find_sets(body, proper.to_set()); //local variables

          var do_body = this.compile(body,
                          [proper.to_set(), free],
                          sets.set_union(s.set_intersect(free)),
                          f.set_union(proper.to_set()),
                          ["return"]);
          var do_close = ["close", 
                           free.size(),
                           this.make_boxes(sets, proper, do_body),
                           next,
                           dotpos];
          return this.collect_free(free, e, do_close);

        case BiwaScheme.Sym("if"):
          if(x.length() < 3 || x.length() > 4)
              throw new BiwaScheme.Error("Invalid if: "+x.to_write());

          var testc=x.second(), thenc=x.third(), elsec=x.fourth();
          var thenc = this.compile(thenc, e, s, f, next);
          var elsec = this.compile(elsec, e, s, f, next);
          x    = testc;
          next = ["test", thenc, elsec];
          break;

        case BiwaScheme.Sym("set!"):
          // error-checking: should have only 3 things
          if(x.length() != 3)
              throw new BiwaScheme.Error("Invalid set!: "+x.to_write());

          var v=x.second(), x=x.third();
          var do_assign = this.compile_lookup(v, e,
            function(n){ return ["assign-local", n, next]; },
            function(n){ return ["assign-free",  n, next]; },
            function(sym){ return ["assign-global",sym, next]; }
          );
          next = do_assign;
          break;

        case BiwaScheme.Sym("call/cc"): 
          var x=x.second();
          var c = ["conti", 
                    (this.is_tail(next) ? (e[0].size() + 1) : 0), //number of args for outer lambda
                    ["argument",
                    ["constant", 1,
                    ["argument",
                      this.compile(x, e, s,f,  
                        (this.is_tail(next) ? ["shift", 1, ["apply"]]
                                            : ["apply"]))]]]];
                  //note: proc for call/cc takes 1 argument (= ["apply", 1])
          return this.is_tail(next) ? c : ["frame", c, next];

        default: 
          //apply 
          //x = (func 1 2) 
          //x.car = func = '(lambda (x) ..) or Symbol
          //x.cdr = args = '(1 2)
          var func = x.car;
          var args = x.cdr;
          var c = this.compile(func, e, s,f,  
                    this.is_tail(next) ? ["shift", args.length(), ["apply"]]
                                       : ["apply"]);

          // VM will push the number of arguments to the stack.
          c = this.compile(args.length(), e, s, f, ["argument", c]);
          for(var p=args; p instanceof BiwaScheme.Pair; p=p.cdr){
            c = this.compile(p.car, e, s, f, ["argument", c]);
          }
          return this.is_tail(next) ? c : ["frame", c, next];
        }
      }
      else{
        return ["constant", x, next];
      }
    }
    //p("result of " + x.inspect() + ":");
    //p(ret);
    //dump({"ret":ret, "x":x, "e":e, "s":s, "next":next, "stack":[]});
//      if(ret == null)
//        throw new BiwaScheme.Bug("compile() exited in unusual way");
//      else
//        return ret;
  },
  run: function(expr){
    return this.compile(expr, [new BiwaScheme.Set(), new BiwaScheme.Set()], new BiwaScheme.Set(), new BiwaScheme.Set(), ["halt"]);
  }
});
BiwaScheme.Compiler.compile = function(expr, next){
  expr = (new BiwaScheme.Interpreter).expand(expr);
  return (new BiwaScheme.Compiler).run(expr, next);
};
//
// pause object (facility to stop/resume interpreting)
//
BiwaScheme.Pause = BiwaScheme.Class.create({
  //new (on_pause: javascript function calling setTimeout, Ajax.Request, ..)
  initialize: function(on_pause){
    this.on_pause = on_pause;
  },

  //save state of interpreter
  set_state: function(intp, x, f, c, s){
    this.interpreter = intp;
    this.x = x;
    this.f = f;
    this.c = c;
    this.s = s;
  },

  //call this when ready (to fire setTimeout, Ajax.Request..)
  ready: function(){
    this.on_pause(this);
  },

  //restart calculation
  resume: function(value){
    return this.interpreter.resume(true, value, this.x, this.f, this.c, this.s)
  }
});

///
/// Call
///

// The class Call is used to invoke scheme closure from 
// library functions.
//
// Call#initialize takes three arguments: proc, args and after.
//   * proc is the scheme closure to invoke.
//   * args is an Array (not list!) of arguments for the invocation.
//   * after is a javascript function which is invoked when 
//     returned from the proc.
//
//     after takes two arguments: ar and intp.
//       * ar is an Array which contains the result of the invocation.
//       * intp is an Interpreter which is running.
//
//     If after returns another Call object, another invocation
//     happens. If after returns a normal value, it is the value
//     of the library function.
//
// example:
//   return new Call(proc, [x, y], function(ar){ ar[0] });
//
BiwaScheme.Call = BiwaScheme.Class.create({
  initialize: function(proc, args, after){
    this.proc = proc;
    this.args = args;
    this.after = after || function(ar){
      // just return result which closure returned
      return ar[0];
    };
  },

  inspect: function(){
    return "#<Call args=" + this.args.inspect() + ">";
  },

  toString: function(){
    return "#<Call>";
  },

  to_write: function(){
    return "#<Call>";
  }
})

//
// Iterator - external iterator for Call.foreach
//
BiwaScheme.Iterator = {
  ForArray: BiwaScheme.Class.create({
    initialize: function(arr){
      this.arr = arr;
      this.i = 0;
    },
    has_next: function(){
      return this.i < this.arr.length;
    },
    next: function(){
      return this.arr[this.i++];
    }
  }),
  ForString: BiwaScheme.Class.create({
    initialize: function(str){
      this.str = str;
      this.i = 0;
    },
    has_next: function(){
      return this.i < this.str.length;
    },
    next: function(){
      return BiwaScheme.Char.get(this.str.charAt(this.i++));
    }
  }),
  ForList: BiwaScheme.Class.create({
    initialize: function(ls){
      this.ls = ls;
    },
    has_next: function(){
      return (this.ls instanceof BiwaScheme.Pair) &&
             this.ls != BiwaScheme.nil;
    },
    next: function(){
      var pair = this.ls;
      this.ls = this.ls.cdr;
      return pair;
    }
  }),
  ForMulti: BiwaScheme.Class.create({
    initialize: function(objs){
      this.objs = objs;
      this.size = objs.length;
      this.iterators = _.map(objs, function(x){
        return BiwaScheme.Iterator.of(x);
      })
    },
    has_next: function(){
      for(var i=0; i<this.size; i++)
        if(!this.iterators[i].has_next())
          return false;
      
      return true;
    },
    next: function(){
      return _.map(this.iterators, function(ite){
        return ite.next();
      })
    }
  }),
  of: function(obj){
    switch(true){
      case (obj instanceof Array):
        return new this.ForArray(obj);
      case (typeof(obj) == "string"):
        return new this.ForString(obj);
      case (obj instanceof BiwaScheme.Pair):
      case (obj === BiwaScheme.nil):
        return new this.ForList(obj);
      default:
        throw new BiwaScheme.Bug("Iterator.of: unknown class: "+BiwaScheme.inspect(obj));
    }
  }
}

//
// Call.foreach - shortcut for successive Calls
//
// Some library functions, such as for-each or map,
// call a closure for each element. Call.foreach is 
// a utility to help defining such methods.
//
// Call.foreach takes a sequence and some callbacks.
// Sequence is an Array, String, or list.
//
// Example:
//   return Call.foreach(sequence, {
//     // before each call
//     call: function(elem){
//       return new Call(proc, [elem]);
//     },
//     // after each call
//     result: function(value, elem){
//       ary.push(value);
//       // you can return a value to terminate the loop
//     },
//     // after all the calls
//     finish: function(){
//       return ary;
//     }
//   });

BiwaScheme.Call.default_callbacks = {
  call: function(x){ return new BiwaScheme.Call(this.proc, [x]) },
  result: function(){},
  finish: function(){}
}
BiwaScheme.Call.foreach = function(obj, callbacks, is_multi){
  is_multi || (is_multi = false);
  _.each(["call", "result", "finish"], function(key){
    if(!callbacks[key])
      callbacks[key] = BiwaScheme.Call.default_callbacks[key];
  })
  
  var iterator = null;
  var x = null;

  var loop = function(ar){
    if(iterator){
      var ret = callbacks["result"](ar[0], x);
      if(ret !== undefined) return ret;
    }
    else{ // first lap
      if(is_multi)
        iterator = new BiwaScheme.Iterator.ForMulti(obj);
      else
        iterator = BiwaScheme.Iterator.of(obj);
    }

    if(!iterator.has_next()){
      return callbacks["finish"]();
    }
    else{
      x = iterator.next();
      var result = callbacks["call"](x);
      result.after = loop;
      return result;
    }
  }
  return loop(null);
}
BiwaScheme.Call.multi_foreach = function(obj, callbacks){
  return BiwaScheme.Call.foreach(obj, callbacks, true);
}

///
/// Interpreter
///

BiwaScheme.Interpreter = BiwaScheme.Class.create({
  initialize: function(on_error){
    this.stack = [] //(make-vector 1000)
    this.on_error = on_error || function(e){};
    this.after_evaluate = function(){};
  },

  inspect: function(){
    return [
      "#<Interpreter: stack size=>",
      this.stack.length, " ",
      "after_evaluate=",
      BiwaScheme.inspect(this.after_evaluate),
      ">"
    ].join("");
  },

  push: function(x, s){
    this.stack[s] = x;
    return s+1;
  },

  //s: depth of stack to save
  //ret: saved(copied) stack 
  save_stack: function(s){
    var v = [];
    for(var i=0; i<s; i++){
      v[i] = this.stack[i];
    }
    return v;
  },

  //v: stack array to restore
  //ret: lenght of restored stack
  restore_stack: function(v){
    var s = v.length;
    for(var i=0; i<s; i++){
      this.stack[i] = v[i];
    }
    return s;
  },

  //s: depth of stack to save
  //n: number of args(for outer lambda) to remove (= 0 unless tail position)
  //ret: closure array
  continuation: function(s, n){
    // note: implementation of this function for final version doesn't exist in 3imp.pdf..
    var ss = this.push(n, s);
    return this.closure(["refer-local", 0,
                          ["nuate", this.save_stack(ss), 
                          ["return"]]], 
                        0,     //n (number of frees)
                        null,  //s (stack position to get frees)
                        -1);   // dotpos
  },

  // shift stack 
  // n: number of items to skip (from stack top)
  // m: number of items to shift
  // s: stack pointer (= index of stack top + 1)
  shift_args: function(n, m, s){
    for(var i = n-1; i >= -1; i--){
      this.index_set(s, i+m+1, this.index(s, i));
    }
    return s-m-1;
  },

  index: function(s, i){
    return this.stack[s-i-2];
  },

  index_set: function(s, i, v){
    this.stack[s-i-2] = v;
  },

  //ret: [body, stack[s-1], stack[s-2], .., stack[s-n], dotpos]
  closure: function(body, n, s, dotpos){
    var v = []; //(make-vector n+1+1)
    v[0] = body;
    for(var i=0; i<n; i++)
      v[i+1] = this.index(s, i-1);
    v[n+1] = dotpos;

    v.closure_p = true;

    return v;
  },

  execute: function(a, x, f, c, s){
    var ret = null;
    try{
      ret = this._execute(a, x, f, c, s);
    }
    catch(e){
      var state = {a:a, x:x, f:f, c:c, s:s, stack:this.stack};
      return this.on_error(e, state);
    }
    return ret;
  },

  run_dump_hook: function(a, x, f, c, s) {
    var dumper;
    var state;


    if (this.dumper) {
      dumper = this.dumper;
    }
    else if (BiwaScheme.Interpreter.dumper) {
      dumper = BiwaScheme.Interpreter.dumper;
    }
    else
      return;

    if (dumper) {
      state = {"a":a,
               "f":f,
               "c":c,
               "s":s,
               "x":x,
               "stack":this.stack};
      dumper.dump(state);
    }
  },

  _execute: function(a, x, f, c, s){
    var ret = null;
    //puts("executing "+x[0]);
    
    while(true){ //x[0] != "halt"){

      this.run_dump_hook(a, x, f, c, s);

      switch(x[0]){
      case "halt":
        return a;
      case "refer-local":
        var n=x[1], x=x[2];
        a = this.index(f, n);
        break;
      case "refer-free":
        var n=x[1], x=x[2];
        a = c[n+1];
        break;
      case "refer-global":
        var sym=x[1], x=x[2];
        if(BiwaScheme.TopEnv.hasOwnProperty(sym))
          var val = BiwaScheme.TopEnv[sym];
        else if(BiwaScheme.CoreEnv.hasOwnProperty(sym))
          var val = BiwaScheme.CoreEnv[sym];
        else
          throw new BiwaScheme.Error("execute: unbound symbol: "+BiwaScheme.inspect(sym));

        a = val;
        break;
      case "indirect":
        var x=x[1];
        a = a[0]; //unboxing
        break;
      case "constant":
        var obj=x[1], x=x[2];
        a = obj;
        break;
      case "close":
        var ox=x;
        var n=ox[1], body=ox[2], x=ox[3], dotpos=ox[4];
        a = this.closure(body, n, s, dotpos);
        s -= n;
        break;
      case "box":
        var n=x[1], x=x[2];
        this.index_set(s, n, [this.index(s, n)]); //boxing
        break;
      case "test":
        var thenc=x[1], elsec=x[2];
        x = ((a!==false) ? thenc : elsec);
        break;
      case "assign-global":
        var name=x[1], x=x[2];
        if(!BiwaScheme.TopEnv.hasOwnProperty(name) &&
           !BiwaScheme.CoreEnv.hasOwnProperty(name))
          throw new BiwaScheme.Error("global variable '"+name+"' is not defined");
        
        BiwaScheme.TopEnv[name] = a;
        a = BiwaScheme.undef;
        break;
      case "assign-local":
        var n=x[1], x=x[2];
        var box = this.index(f, n);
        box[0] = a;
        a = BiwaScheme.undef;
        break;
      case "assign-free":
        var n=x[1], x=x[2];
        var box = c[n+1];
        box[0] = a;
        a = BiwaScheme.undef;
        break;
      case "conti":
        var n=x[1], x=x[2];
        a = this.continuation(s, n);
        break;
      case "nuate":
        var stack=x[1], x=x[2];
        s = this.restore_stack(stack);
        break;
      case "frame":
        var ret = x[2];
        x = x[1];
        s = this.push(ret, this.push(f, this.push(c, s)));
        break;
      case "argument":
        var x=x[1];
        s = this.push(a, s);
        break;
      case "shift":
        var n=x[1], x=x[2];

        // the number of arguments in the last call
        var n_args = this.index(s, n);  

        s = this.shift_args(n, n_args, s);
        break;
      case "apply": //extended: n_args as second argument
        var func = a; //, n_args = x[1];

        // the number of arguments in the last call is
        // pushed to the stack.
        var n_args = this.index(s, -1);
        if(func instanceof Array){ //closure
          a = func;
          x = func[0];

          // The position of dot in the parameter list.
          var dotpos = func[func.length-1];

          if (dotpos >= 0) {
            // The dot is found
            // ----------------
            // => Process the &rest args: packing the rest args into a list.
            var ls = BiwaScheme.nil;
            for (var i=n_args; --i>=dotpos; ) {
              ls = new BiwaScheme.Pair(this.index(s, i), ls);
            }
            if (dotpos >= n_args) {
              // No rest argument is passed to this closure.
              // However, the closure expects the caller passes the rest argument.
              // In such case this VM prepares an empty list as the rest argument.
              // --------------------------------------------------------------
              // => We extend the stack to put the empty list.
              for(var i = -1; i < n_args; i++){
                this.index_set(s, i-1, this.index(s, i));
              }
              s++;
              // => Update the number of arguments
              this.index_set(s, -1, this.index(s, -1) + 1);  
            }
            this.index_set(s, dotpos, ls);
          }
          f = s;
          c = a;
        }
        else if(func instanceof Function){ // Apply JavaScript function
          // load arguments from stack
          var args = [];
          for(var i=0; i<n_args; i++) 
            args.push(this.index(s, i));

          // invoke the function
          var result = func(args, this);

          if(result instanceof BiwaScheme.Pause){
            // it requested the interpreter to suspend
            var pause = result;
            pause.set_state(this, ["return"], f, c, s);
            pause.ready();
            return pause;
          }
          else if(result instanceof BiwaScheme.Call){
            // it requested the interpreter to call a scheme closure

            //   [frame,
            //     [constant... (args)
            //     [constant, proc
            //     [apply]]]]
            //   [frame,
            //     [constant, after
            //     [apply 1]]]]
            //   x
            var call_after = ["frame",
                               ["argument",
                               ["constant", 1,
                               ["argument",
                               ["constant", result.after,
                               ["apply"]]]]],
                             ["return"]];
            var call_proc = ["constant", result.args.length,
                            ["argument",
                            ["constant", result.proc, 
                            ["apply", result.args.length]]]];
            var push_args = _.inject(result.args, function(opc, arg){
              // (foo 1 2) => first push 2, then 1
              //   [constant 2 ... [constant 1 ... ]
              return ["constant", arg, 
                     ["argument",
                     opc]];
            }, call_proc);
            x = ["frame",
                  push_args,
                call_after]
          }
          else{
            // the JavaScript function returned a normal value
            a = result;
            x = ["return"];
          }
        }
        else{
          // unknown function type
          throw new BiwaScheme.Error(BiwaScheme.inspect(func) + " is not a function");
        }
        break;
      case "return":
        var n=this.index(s, -1);
        var ss=s-n;
        x = this.index(ss, 0),
        f = this.index(ss, 1),
        c = this.index(ss, 2),
        s = ss-3-1;
        break;
      default:
        throw new BiwaScheme.Bug("unknown opecode type: "+x[0]);
      }
    }

//      if(ret === null)
//        throw new BiwaScheme.Bug("interpreter exited in unusual way");
//      else
//        return ret;
    return a
  },

  // expand macro forms (recursively)
  expand: function(x, flag){
    flag || (flag = {})
    var ret = null;
    if(x instanceof BiwaScheme.Symbol){
      ret = x;
    }
    else if(x instanceof BiwaScheme.Pair){
      switch(x.car){
      case BiwaScheme.Sym("define"):
        var left = x.cdr.car, exp = x.cdr.cdr;
        ret = new BiwaScheme.Pair(BiwaScheme.Sym("define"),
                new BiwaScheme.Pair(left, this.expand(exp, flag)));
        break;
      case BiwaScheme.Sym("begin"):
        ret = new BiwaScheme.Pair(BiwaScheme.Sym("begin"), this.expand(x.cdr, flag));
        break;
      case BiwaScheme.Sym("quote"):
        ret = x;
        break;
      case BiwaScheme.Sym("lambda"):
        var vars=x.cdr.car, body=x.cdr.cdr;
        ret = new BiwaScheme.Pair(BiwaScheme.Sym("lambda"),
                new BiwaScheme.Pair(vars, this.expand(body, flag)));
        break;
      case BiwaScheme.Sym("if"):
        var testc=x.second(), thenc=x.third(), elsec=x.fourth();
        if (elsec == BiwaScheme.inner_of_nil){
          elsec = BiwaScheme.undef;
        }
        ret = BiwaScheme.List(BiwaScheme.Sym("if"),
                              this.expand(testc, flag),
                              this.expand(thenc, flag),
                              this.expand(elsec, flag));
        break;
      case BiwaScheme.Sym("set!"):
        var v=x.second(), x=x.third();
        ret = BiwaScheme.List(BiwaScheme.Sym("set!"), v, this.expand(x, flag));
        break;
      case BiwaScheme.Sym("call-with-current-continuation"): 
      case BiwaScheme.Sym("call/cc"): 
        var x=x.second();
        ret = BiwaScheme.List(BiwaScheme.Sym("call/cc"), this.expand(x, flag));
        break;
      default: //apply
        // if x is a macro call ...
        if(x.car instanceof BiwaScheme.Symbol &&
            BiwaScheme.TopEnv[x.car.name] instanceof BiwaScheme.Syntax){
          var transformer = BiwaScheme.TopEnv[x.car.name];
          flag["modified"] = true;
          ret = transformer.transform(x);

          if(BiwaScheme.Debug){
            var before = BiwaScheme.to_write(x);
            var after = BiwaScheme.to_write(ret);
            if(before != after) puts("expand: " + before + " => " + after)
          }

          var fl;
          for(;;){
            ret = this.expand(ret, fl={});
            if(!fl["modified"]) 
              break;
          }
        }
        else if(x == BiwaScheme.nil)
          ret = BiwaScheme.nil;
        else{
          ret = new BiwaScheme.Pair(this.expand(x.car, flag), BiwaScheme.shallow_array_to_list(_.map(x.cdr.to_array(), _.bind(function(item){ return this.expand(item, flag); }, this))));
        }
      }
    }
    else{
      ret = x;
    }
    return ret;
  },

  evaluate: function(str, after_evaluate){
    this.parser = new BiwaScheme.Parser(str);
    this.compiler = new BiwaScheme.Compiler();
    if(after_evaluate) 
      this.after_evaluate = after_evaluate;

    if(BiwaScheme.Debug) puts("executing: " + str);
     
    this.is_top = true;
    this.file_stack = [];
    return this.resume(false);
  },

  resume: function(is_resume, a, x, f, c, s){
    var ret = BiwaScheme.undef;

    for(;;){
      if(is_resume){
        ret = this.execute(a, x, f, c, s);
        is_resume = false;
      }
      else{
        if(!this.parser) break; // adhoc: when Pause is used via invoke_closure
        var expr = this.parser.getObject();
        if(expr === BiwaScheme.Parser.EOS) break;

        // expand
        expr = this.expand(expr);

        // compile
        var opc = this.compiler.run(expr);
        //if(BiwaScheme.Debug) p(opc);

        // execute
        ret = this.execute(expr, opc, 0, [], 0);
      }

      if(ret instanceof BiwaScheme.Pause){ //suspend evaluation
        return ret;
      }
    }

    // finished executing all forms
    this.after_evaluate(ret);
    return ret;
  },

  invoke_closure: function(closure, args){
    args || (args = []);
    var n_args  = args.length;

    var x = ["constant", n_args, ["argument", ["constant", closure, ["apply"]]]]
    for(var i=0; i<n_args; i++)
      x = ["constant", args[i], ["argument", x]]

    return this.execute(closure, ["frame", x, ["halt"]], 0, closure, 0);
  },

  // only compiling (for debug use only)
  compile: function(str){
    var obj = BiwaScheme.Interpreter.read(str);
    var opc = BiwaScheme.Compiler.compile(obj);
    return opc;
  }
});
BiwaScheme.Interpreter.read = function(str){
  var parser = new BiwaScheme.Parser(str);
  var r      = parser.getObject();
  return (r == BiwaScheme.Parser.EOS)? BiwaScheme.eof: r;
};
///
/// infra.js - Basis for library functions
///

//
// define_*func - define library functions
//
BiwaScheme.check_arity = function(len, min, max){
  var fname = arguments.callee.caller
                ? arguments.callee.caller.fname
                : "(?)";
  if(len < min){
    if(max && max == min)
      throw new BiwaScheme.Error(fname+": wrong number of arguments (expected: "+min+" got: "+len+")");
    else
      throw new BiwaScheme.Error(fname+": too few arguments (at least: "+min+" got: "+len+")");
  }
  else if(max && max < len)
    throw new BiwaScheme.Error(fname+": too many arguments (at most: "+max+" got: "+len+")");
}
BiwaScheme.define_libfunc = function(fname, min, max, func, is_raw){
  var f = function(ar, intp){
    BiwaScheme.check_arity(ar.length, min, max);
    var result = func(ar, intp);
    if (is_raw) {
      return result;
    }
    else{
      if (result === undefined){
        throw new BiwaScheme.Bug("library function " + 
                                 "`" + fname + "'" +
                                 " returned JavaScript's undefined");
      }
      else if (result === null){
        throw new BiwaScheme.Bug("library function " +
                                 "`" + fname + "'" + 
                                 " returned JavaScript's null");
      }
      else {
        return result;
      }
    }
  };

  func["fname"] = fname; // for assert_*
  f["fname"]    = fname; // for check_arity
  f["inspect"] = function(){ return this.fname; }
  BiwaScheme.CoreEnv[fname] = f;
}
BiwaScheme.alias_libfunc = function(fname, aliases) {
  if (BiwaScheme.CoreEnv[fname]) {
    if (_.isArray(aliases)) {
      _.map(aliases, function(a) { BiwaScheme.alias_libfunc(fname, a); });
    } else if (_.isString(aliases)) {
      BiwaScheme.CoreEnv[aliases] = BiwaScheme.CoreEnv[fname];
    } else {
      throw new BiwaScheme.Bug("bad alias for library function " +
                               "`" + fname + "': " + aliases.toString());
    }
  } else {
    throw new BiwaScheme.Bug("library function " +
                             "`" + fname + "'" +
                             " does not exist, so can't alias it.");
  }
};
BiwaScheme.define_libfunc_raw = function(fname, min, max, func){
  BiwaScheme.define_libfunc(fname, min, max, func, true);
}
BiwaScheme.define_syntax = function(sname, func) {
  var s = new BiwaScheme.Syntax(sname, func);
  BiwaScheme.TopEnv[sname] = s;
}
BiwaScheme.define_scmfunc = function(fname, min, max, str){
  (new Interpreter).evaluate("(define "+fname+" "+str+"\n)");
}

//  define_scmfunc("map+", 2, null, 
//    "(lambda (proc ls) (if (null? ls) ls (cons (proc (car ls)) (map proc (cdr ls)))))");

//
// assertions - type checks
//
var make_assert = function(check){
  return function(/*args*/){
    var fname = arguments.callee.caller
                  ? arguments.callee.caller.fname 
                  : "";
    check.apply(this, [fname].concat(_.toArray(arguments)));
  }
}
var make_simple_assert = function(type, test){
  return make_assert(function(fname, obj, opt){
    option = opt ? ("("+opt+")") : ""
    if(!test(obj)){
      throw new BiwaScheme.Error(fname + option + ": " +
                                 type + " required, but got " +
                                 BiwaScheme.to_write(obj));
    }
  })
}

var assert_number = make_simple_assert("number", function(obj){
  return typeof(obj) == 'number' || (obj instanceof BiwaScheme.Complex);
});

var assert_integer = make_simple_assert("integer", function(obj){
  return typeof(obj) == 'number' && (obj % 1 == 0)
});

var assert_real = make_simple_assert("real number", function(obj){
  return typeof(obj) == 'number';
});

var assert_between = make_assert(function(fname, obj, from, to){
  if( typeof(obj) != 'number' || obj != Math.round(obj) ){
    throw new BiwaScheme.Error(fname + ": " +
                               "number required, but got " +
                               BiwaScheme.to_write(obj));
  }

  if( obj < from || to < obj ){
    throw new BiwaScheme.Error(fname + ": " + 
                               "number must be between " + 
                               from + " and " + to + ", but got " +
                               BiwaScheme.to_write(obj));
  }
});

var assert_string = make_simple_assert("string", _.isString);

var assert_char = make_simple_assert("character", BiwaScheme.isChar);
var assert_symbol = make_simple_assert("symbol", BiwaScheme.isSymbol);
var assert_port = make_simple_assert("port", BiwaScheme.isPort);
var assert_pair = make_simple_assert("pair", BiwaScheme.isPair);
var assert_list = make_simple_assert("list", BiwaScheme.isList);
var assert_vector = make_simple_assert("vector", BiwaScheme.isVector);

var assert_hashtable = make_simple_assert("hashtable",
                                          BiwaScheme.isHashtable);
var assert_mutable_hashtable = make_simple_assert("mutable hashtable", 
                                            BiwaScheme.isMutableHashtable);

var assert_record = make_simple_assert("record",
                                          BiwaScheme.isRecord);
var assert_record_td = make_simple_assert("record type descriptor",
                                          BiwaScheme.isRecordTD);
var assert_record_cd = make_simple_assert("record constructor descriptor",
                                          BiwaScheme.isRecordCD);

var assert_function = make_simple_assert("JavaScript function", 
                                         _.isFunction);
var assert_closure = make_simple_assert("scheme function", 
                                        BiwaScheme.isClosure);
var assert_procedure = make_simple_assert("scheme/js function", function(obj){
  return BiwaScheme.isClosure(obj) || _.isFunction(obj);
});

var assert_date = make_simple_assert("date", function(obj){
  // FIXME: this is not accurate (about cross-frame issue)
  // https://prototype.lighthouseapp.com/projects/8886/tickets/443
  return obj instanceof Date;
});

//var assert_instance_of = make_assert(function(fname, type, obj, klass){
//  if(!(obj instanceof klass)){
//    throw new BiwaScheme.Error(fname + ": " +
//                               type + " required, but got " +
//                               BiwaScheme.to_write(obj));
//  }
//});

var assert = make_assert(function(fname, success, message){
  if(!success){
    throw new BiwaScheme.Error(fname+": "+message);
  }
});


//
// R6RS Base library
//

if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  /* --------------------------------------- namespace webscheme */ 

  ///
  /// R6RS Base library
  ///
  
  //
  //        11.4  Expressions
  //
  //            11.4.1  Quotation
  //(quote)
  //            11.4.2  Procedures
  //(lambda)
  //            11.4.3  Conditionaar
  //(if)
  //            11.4.4  Assignments
  //(set!)
  //            11.4.5  Derived conditionaar

  define_syntax("cond", function(x){
    var clauses = x.cdr;
    if(!(clauses instanceof Pair) || clauses === nil){
      throw new Error("malformed cond: cond needs list but got " +
                      to_write_ss(clauses));
    }
    // TODO: assert that clauses is a proper list

    var ret = null;
    _.each(clauses.to_array().reverse(), function(clause){
      if(!(clause instanceof Pair)){
        throw new Error("bad clause in cond: " + to_write_ss(clause));
      }

      if(clause.car === Sym("else")){ 
        if(ret !== null){
          throw new Error("'else' clause of cond followed by more clauses: " +
                          to_write_ss(clauses));
        }
        else if(clause.cdr === nil){
          // pattern A: (else)
          //  -> #f            ; not specified in R6RS...?
          ret = false;
        }
        else if(clause.cdr.cdr === nil){
          // pattern B: (else expr) 
          //  -> expr
          ret = clause.cdr.car;
        }
        else{
          // pattern C: (else expr ...)
          //  -> (begin expr ...)
          ret = new Pair(Sym("begin"), clause.cdr);
        }
      } 
      else if(ret === null){
        // pattern D: no else clause
        //  -> #<undef>
        ret = BiwaScheme.undef;
      }
      else{
        var test = clause.car;
        if(clause.cdr === nil){
          // pattern 1: (test)
          //  -> (or test ret)
          ret = List(Sym("or"), test, ret);
        }
        else if (clause.cdr.cdr === nil){
          // pattern 2: (test expr)
          //  -> (if test expr ret)
          ret = List(Sym("if"), test, clause.cdr.car, ret);
        }
        else if(clause.cdr.car === Sym("=>")){
          // pattern 3: (test => expr)
          //  -> (let ((#<gensym1> test)) 
          //       (if test (expr #<gensym1>) ret))
          var test = clause.car, expr = clause.cdr.cdr.car;
          var tmp_sym = BiwaScheme.gensym();

          ret = List(Sym("let"), 
                     List( List(tmp_sym, test) ),
                     List(Sym("if"), test, List(expr, tmp_sym), ret));
        }
        else{
          // pattern 4: (test expr ...)
          //  -> (if test (begin expr ...) ret)
          ret = List(Sym("if"), test,
                     new Pair(Sym("begin"), clause.cdr),
                     ret);
        }
      }
    });
    return ret;
  });

  define_syntax("case", function(x){
    var tmp_sym = BiwaScheme.gensym();

    if(x.cdr === nil){
      throw new Error("case: at least one clause is required");
    }
    else if(!(x.cdr instanceof Pair)){
      throw new Error("case: proper list is required");
    }
    else{
      // (case key clauses ....)
      //  -> (let ((#<gensym1> key))
      var key = x.cdr.car;
      var clauses = x.cdr.cdr;

      var ret = undefined;
      _.each(clauses.to_array().reverse(), function(clause){
        if(clause.car === Sym("else")){
          // pattern 0: (else expr ...)
          //  -> (begin expr ...)
          if(ret === undefined){
            ret = new Pair(Sym("begin"), clause.cdr);
          }
          else{
            throw new Error("case: 'else' clause followed by more clauses: " +
                            to_write_ss(clauses));
          }
        }
        else{
          // pattern 1: ((datum ...) expr ...)
          //  -> (if (or (eqv? key (quote d1)) ...) (begin expr ...) ret)
          ret = List(
            Sym("if"),
            new Pair(Sym("or"), array_to_list(_.map(clause.car.to_array(), function(d){
                return List(Sym("eqv?"),
                            tmp_sym,
                            List(Sym("quote"), d));
            }))),
            new Pair(Sym("begin"), clause.cdr),
            ret
          );
        }
      });
      return new Pair(Sym("let1"),
               new Pair(tmp_sym,
                 new Pair(key,
                   new Pair(ret, nil))));
    }
  });

  define_syntax("and", function(x){
    // (and a b c) => (if a (if b c #f) #f)
    //todo: check improper list
    if(x.cdr == nil) return true;

    var objs = x.cdr.to_array();
    var i = objs.length-1;
    var t = objs[i];
    for(i=i-1; i>=0; i--)
      t = List(Sym("if"), objs[i], t, false);

    return t;
  })

  define_syntax("or", function(x){
    // (or a b c) => (if a a (if b b (if c c #f)))
    //todo: check improper list

    var objs = x.cdr.to_array()
    var f = false;
    for(var i=objs.length-1; i>=0; i--)
      f = List(Sym("if"), objs[i], objs[i], f);

    return f;
  })

  //            11.4.6  Binding constructs
  define_syntax("let", function(x){
    //(let ((a 1) (b 2)) (print a) (+ a b))
    //=> ((lambda (a b) (print a) (+ a b)) 1 2)
    var name = null;
    if (x.cdr.car instanceof Symbol) {
      name = x.cdr.car;
      x = x.cdr;
    }
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if(!(binds instanceof Pair)) 
      throw new Error("let: need a pair for bindings: got "+to_write(binds));

    var vars = nil, vals = nil;
    for(var p=binds; p instanceof Pair; p=p.cdr){
      vars = new Pair(p.car.car, vars);
      vals = new Pair(p.car.cdr.car, vals);
    }

    var lambda = null;
    if (name) {
      // (let loop ((a 1) (b 2)) body ..)
      //=> (letrec ((loop (lambda (a b) body ..))) (loop 1 2))
      vars = array_to_list(vars.to_array().reverse());
      vals = array_to_list(vals.to_array().reverse());

      var body_lambda = new Pair(Sym("lambda"), new Pair(vars, body));
      var init_call = new Pair(name, vals);

      lambda = List(Sym("letrec"),
                    new Pair(List(name, body_lambda), nil),
                    init_call);
    }
    else {
      lambda = new Pair(new Pair(Sym("lambda"), 
                                 new Pair(vars, body)), 
                        vals);
    }
    return lambda;
  })

  define_syntax("let*", function(x){
    //(let* ((a 1) (b a)) (print a) (+ a b))
    //-> (let ((a 1)) 
    //     (let ((b a)) (print a) (+ a b)))
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if(!(binds instanceof Pair)) 
      throw new Error("let*: need a pair for bindings: got "+to_write(binds));

    var ret = null;
    _.each(binds.to_array().reverse(), function(bind){
      ret = new Pair(Sym("let"), 
               new Pair(new Pair(bind, nil),
                 ret == null ? body : new Pair(ret, nil)));
    })
    return ret;
  })

  var expand_letrec_star = function(x){
    var binds = x.cdr.car, body = x.cdr.cdr;
    
    if(!(binds instanceof Pair)) 
      throw new Error("letrec*: need a pair for bindings: got "+to_write(binds));

    var ret = body;
    _.each(binds.to_array().reverse(), function(bind){
      ret = new Pair(new Pair(Sym("set!"), bind),
              ret);
    })
    var letbody = nil;
    _.each(binds.to_array().reverse(), function(bind){
      letbody = new Pair(new Pair(bind.car, 
                           new Pair(BiwaScheme.undef, nil)),
                  letbody);
    })
    return new Pair(Sym("let"),
             new Pair(letbody,
               ret));
  }
  define_syntax("letrec", expand_letrec_star);
  define_syntax("letrec*", expand_letrec_star);

  define_syntax("let-values", function(x) {
    // (let-values (((a b) (values 1 2))
    //               ((c d . e) (values 3 4 a)))
    //              (print a b c d e))
    // =>
    // (let ((#<gensym1> (lambda () (values 1 2)))
    //       (#<gensym2> (lambda () (values 3 4 a))))
    //   (let*-values (((a b) #<gensym1>)
    //                 ((c d . e) #<gensym2>))
    //                 (print a b c d e)))
      var mv_bindings = x.cdr.car;
      var body = x.cdr.cdr;
      var ret = null;
      
      var let_bindings = nil;
      var let_star_values_bindings = nil;
      _.each(mv_bindings.to_array().reverse(), function (item) {
	  var init = item.cdr.car;
	  var tmpsym = BiwaScheme.gensym()
	  var binding = new Pair(tmpsym, 
				 new Pair(
					  new Pair(Sym("lambda"), new Pair(nil, 
									   new Pair(init, nil))),
					  nil));
	  let_bindings = new Pair(binding, let_bindings);
	  
	  var formals = item.car;
	  let_star_values_bindings = new Pair(new Pair (formals, new Pair(new Pair(tmpsym, nil), nil)),
					      let_star_values_bindings);
      });

      var let_star_values = new Pair(Sym("let*-values"),
				     new Pair(let_star_values_bindings,
					      body));
      ret = new Pair(Sym("let"), 
		     new Pair(let_bindings,
			      new Pair (let_star_values, nil)));
      return ret;
      
  });

  //let*-values
  define_syntax("let*-values", function(x){
    // (let*-values (((a b) (values 1 2))
    //               ((c d . e) (values 3 4 a)))
    //   (print a b c d e))
    // -> (call-with-values
    //      (lambda () (values 1 2))
    //      (lambda (a b)
    //        (call-with-values
    //          (lambda () (values 3 4 a))
    //          (lambda (c d . e) 
    //            (print a b c d e)))))
    var mv_bindings = x.cdr.car;
    var body = x.cdr.cdr;

    var ret = null;

    _.each(mv_bindings.to_array().reverse(), function(item){
      var formals = item.car, init = item.cdr.car;
      ret = new Pair(Sym("call-with-values"),
              new Pair(new Pair(Sym("lambda"),
                         new Pair(nil,
                           new Pair(init, nil))),
                new Pair(new Pair(Sym("lambda"),
                           new Pair(formals,
                             (ret == null ? body
                                          : new Pair(ret, nil)))), nil)));
    });
    return ret;
  });
  //            11.4.7  Sequencing
  //(begin)

  //        
  //        11.5  Equivalence predicates
  //
  BiwaScheme.eq = function(a, b){
    return a === b;
  };
   // TODO: Records (etc.)
  BiwaScheme.eqv = function(a, b){
    return a == b && (typeof(a) == typeof(b));
  };
  
  define_libfunc("eqv?", 2, 2, function(ar){
    return BiwaScheme.eqv(ar[0], ar[1]);
  })
  define_libfunc("eq?", 2, 2, function(ar){
    return BiwaScheme.eq(ar[0], ar[1]);
  })
  define_libfunc("equal?", 2, 2, function(ar){
    //TODO: must terminate for cyclic objects
    return to_write(ar[0]) == to_write(ar[1]);
  })

  //
  //        11.6  Procedure predicate
  //
  //"procedure?", 1, 1
  define_libfunc("procedure?", 1, 1, function(ar){
    return ((ar[0] instanceof Array) && (ar[0].closure_p === true)
	    || (typeof ar[0] == "function"));
  })

  //
  //        11.7  Arithmetic
  //

  //            11.7.1  Propagation of exactness and inexactness
  //            11.7.2  Representability of infinities and NaNs
  //            11.7.3  Semantics of common operations
  //                11.7.3.1  Integer division
  //                11.7.3.2  Transcendental functions
  //(no functions are introduced by above sections)

  //
  //            11.7.4  Numerical operations
  //
  
  //                11.7.4.1  Numerical type predicates
  define_libfunc("number?", 1, 1, function(ar){
    return (typeof(ar[0]) == 'number') ||
           (ar[0] instanceof Complex)  ||
           (ar[0] instanceof Rational);
  });
  define_libfunc("complex?", 1, 1, function(ar){
    return (ar[0] instanceof Complex);
  });
  define_libfunc("real?", 1, 1, function(ar){
    return (typeof(ar[0]) == 'number');
  });
  define_libfunc("rational?", 1, 1, function(ar){
    return (ar[0] instanceof Rational);
  });
  define_libfunc("integer?", 1, 1, function(ar){
    return typeof(ar[0]) == 'number'  && 
           ar[0] == Math.round(ar[0]) &&
           ar[0] != Infinity          &&
           ar[0] != -Infinity;
  });

//(real-valued? obj)    procedure 
//(rational-valued? obj)    procedure 
//(integer-valued? obj)    procedure 
//
//(exact? z)    procedure 
//(inexact? z)    procedure

  //                11.7.4.2  Generic conversions
  //
//(inexact z)    procedure 
//(exact z)    procedure
//
  //                11.7.4.3  Arithmetic operations

  //inf & nan: ok (for this section)
  define_libfunc("=", 2, null, function(ar){
    var v = ar[0];
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(ar[i] != v) return false;
    }
    return true;
  });
  define_libfunc("<", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] < ar[i])) return false;
    }
    return true;
  });
  define_libfunc(">", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] > ar[i])) return false;
    }
    return true;
  });
  define_libfunc("<=", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] <= ar[i])) return false;
    }
    return true;
  });
  define_libfunc(">=", 2, null, function(ar){
    assert_number(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_number(ar[i]);
      if(!(ar[i-1] >= ar[i])) return false;
    }
    return true;
  });

  define_libfunc("zero?", 1, 1, function(ar){
    assert_number(ar[0]);
    return ar[0] === 0; 
  });
  define_libfunc("positive?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] > 0);
  });
  define_libfunc("negative?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] < 0);
  });
  define_libfunc("odd?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] % 2 == 1) || (ar[0] % 2 == -1);
  })
  define_libfunc("even?", 1, 1, function(ar){
    assert_number(ar[0]);
    return ar[0] % 2 == 0;
  })
  define_libfunc("finite?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] != Infinity) && (ar[0] != -Infinity) && !isNaN(ar[0]);
  })
  define_libfunc("infinite?", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] == Infinity) || (ar[0] == -Infinity);
  })
  define_libfunc("nan?", 1, 1, function(ar){
    assert_number(ar[0]);
    return isNaN(ar[0]);
  })
  define_libfunc("max", 2, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_number(ar[i]);

    return Math.max.apply(null, ar)
  });
  define_libfunc("min", 2, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_number(ar[i]);

    return Math.min.apply(null, ar);
  });

  define_libfunc("+", 0,null, function(ar){
    var n = 0;
    for(var i=0; i<ar.length; i++){
      assert_number(ar[i]);
      n+=ar[i];
    }
    return n;
  });
  define_libfunc("*", 0,null, function(ar){
    var n = 1;
    for(var i=0; i<ar.length; i++){
      assert_number(ar[i]);
      n*=ar[i];
    }
    return n;
  });
  define_libfunc("-", 1,null, function(ar){
    var len = ar.length;
    assert_number(ar[0]);

    if(len == 1)
      return -ar[0];
    else{
      var n = ar[0];
      for(var i=1; i<len; i++){
        assert_number(ar[i]);
        n-=ar[i];
      }
      return n;
    }
  });
  //for r6rs specification, (/ 0 0) or (/ 3 0) raises '&assertion exception'
  define_libfunc("/", 1,null, function(ar){
    var len = ar.length;
    assert_number(ar[0]);

    if(len == 1)
      return 1/ar[0];
    else{
      var n = ar[0];
      for(var i=1; i<len; i++){
        assert_number(ar[i]);
        n/=ar[i];
      }
      return n;
    }
  });

  define_libfunc("abs", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.abs(ar[0]);
  });

  var div = function(n, m){
    return Math.floor(n / m);
  }
  var mod = function(n, m){
    return n - Math.floor(n / m) * m;
  }
  var div0 = function(n, m){
    return (n > 0) ? Math.floor(n / m) : Math.ceil(n / m);
  }
  var mod0 = function(n, m){
    return (n > 0) ? n - Math.floor(n / m) * m 
                   : n - Math.ceil(n / m) * m;
  }
  define_libfunc("div0-and-mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Values([div(ar[0], ar[1]), mod(ar[0], ar[1])]);
  })
  define_libfunc("div", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return div(ar[0], ar[1]);
  })
  define_libfunc("mod", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return mod(ar[0], ar[1]);
  })
  define_libfunc("div0-and-mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Values([div0(ar[0], ar[1]), mod0(ar[0], ar[1])]);
  })
  define_libfunc("div0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return div0(ar[0], ar[1]);
  })
  define_libfunc("mod0", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return mod0(ar[0], ar[1]);
  })

//(gcd n1 ...)    procedure 
//(lcm n1 ...)    procedure 

  define_libfunc("numerator", 1, 1, function(ar){
    assert_number(ar[0]);
    if(ar[0] instanceof Rational)
      return ar[0].numerator;
    else
      throw new Bug("todo");
  })
  define_libfunc("denominator", 1, 1, function(ar){
    assert_number(ar[0]);
    if(ar[0] instanceof Rational)
      return ar[0].denominator;
    else
      throw new Bug("todo");
  })
  define_libfunc("floor", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.floor(ar[0]);
  })
  define_libfunc("ceiling", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.ceil(ar[0]);
  })
  define_libfunc("truncate", 1, 1, function(ar){
    assert_number(ar[0]);
    return (ar[0] < 0) ? Math.ceil(ar[0]) : Math.floor(ar[0]);
  })
  define_libfunc("round", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.round(ar[0]);
  })

//(rationalize x1 x2)    procedure 

  define_libfunc("exp", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.exp(ar[0]);
  })
  define_libfunc("log", 1, 2, function(ar){
    var num = ar[0], base = ar[1];
    assert_number(num);

    if(base){ // log b num == log e num / log e b
      assert_number(base);
      return Math.log(num) / Math.log(b)
    }
    else
      return Math.log(num);
  })
  define_libfunc("sin", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.sin(ar[0]);
  })
  define_libfunc("cos", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.cos(ar[0]);
  })
  define_libfunc("tan", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.tan(ar[0]);
  })
  define_libfunc("asin", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.asin(ar[0]);
  })
  define_libfunc("acos", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.acos(ar[0]);
  })
  define_libfunc("atan", 1, 2, function(ar){
    assert_number(ar[0]);
    if(ar[1]){
      assert_number(ar[1]);
      return Math.atan2(ar[0], ar[1]);
    }
    else
      return Math.atan(ar[0]);
  })
  define_libfunc("sqrt", 1, 1, function(ar){
    assert_number(ar[0]);
    return Math.sqrt(ar[0]);
  })
  define_libfunc("exact-integer-sqrt", 1, 1, function(ar){
    assert_number(ar[0]);
    var sqrt_f = Math.sqrt(ar[0]);
    var sqrt_i = sqrt_f - (sqrt_f % 1);
    var rest   = ar[0] - sqrt_i * sqrt_i;

    return new Values([sqrt_i, rest]);
  })
  define_libfunc("expt", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return Math.pow(ar[0], ar[1]);
  })
  define_libfunc("make-rectangular", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return new Complex(ar[0], ar[1]);
  })
  define_libfunc("make-polar", 2, 2, function(ar){
    assert_number(ar[0]);
    assert_number(ar[1]);
    return Complex.from_polar(ar[0], ar[1]);
  })
  define_libfunc("real-part", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).real;
  })
  define_libfunc("imag-part", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).imag;
  })
  define_libfunc("magnitude", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).magnitude();
  })
  define_libfunc("angle", 1, 1, function(ar){
    assert_number(ar[0]);
    return Complex.assure(ar[0]).angle();
  })

  //
  //                11.7.4.4  Numerical Input and Output
  //
  define_libfunc("number->string", 1, 3, function(ar){
    var z = ar[0], radix = ar[1], precision = ar[2];
    if(precision)
      throw new Bug("number->string: precision is not yet implemented");
    
    radix = radix || 10;  //TODO: check radix is 2, 8, 10, or 16.
    return z.toString(radix);
  })
  define_libfunc("string->number", 1, 3, function(ar){
    var s = ar[0], radix = ar[1] || 10;
    switch(s){
      case "+inf.0": return Infinity;
      case "-inf.0": return -Infinity;
      case "+nan.0": return NaN;
      default:       if(s.match(/[eE.]/))
                       return parseFloat(s);
                     else
                       return parseInt(s, radix);

    }
  })

  //
  //        11.8  Booleans
  //

  define_libfunc("not", 1, 1, function(ar){
    return (ar[0] === false) ? true : false;
  });
  define_libfunc("boolean?", 1, 1, function(ar){
    return (ar[0] === false || ar[0] === true) ? true : false;
  });
  define_libfunc("boolean=?", 2, null, function(ar){
    var len = ar.length;
    for(var i=1; i<len; i++){
      if(ar[i] != ar[0]) return false;
    }
    return true;
  });

  //        11.9  Pairs and lists

  define_libfunc("pair?", 1, 1, function(ar){
    return (ar[0] instanceof Pair) ? true : false;
  });
  define_libfunc("cons", 2, 2, function(ar){
    return new Pair(ar[0], ar[1]);
  });
  define_libfunc("car", 1, 1, function(ar){
    //should raise &assertion for '()...
    if(!(ar[0] instanceof Pair)) throw new Error("Attempt to apply car on " + ar[0]);
    return ar[0].car;
  });
  define_libfunc("cdr", 1, 1, function(ar){
    //should raise &assertion for '()...
    if(!(ar[0] instanceof Pair)) throw new Error("Attempt to apply cdr on " + ar[0]);
    return ar[0].cdr;
  });
  define_libfunc("set-car!", 2, 2, function(ar){
    if(!(ar[0] instanceof Pair)) throw new Error("Attempt to apply set-car! on " + ar[0]);
    ar[0].car = ar[1];
    return BiwaScheme.undef;
  });
  define_libfunc("set-cdr!", 2, 2, function(ar){
    if(!(ar[0] instanceof Pair)) throw new Error("Attempt to apply set-cdr! on " + ar[0]);
    ar[0].cdr = ar[1];
    return BiwaScheme.undef;
  });

  define_libfunc("caar", 1, 1, function(ar){ return ar[0].car.car; });
  define_libfunc("cadr", 1, 1, function(ar){ return ar[0].cdr.car; });
  define_libfunc("cdar", 1, 1, function(ar){ return ar[0].car.cdr; });
  define_libfunc("cddr", 1, 1, function(ar){ return ar[0].cdr.cdr; });
  define_libfunc("caaar", 1, 1, function(ar){ return ar[0].car.car.car; });
  define_libfunc("caadr", 1, 1, function(ar){ return ar[0].cdr.car.car; });
  define_libfunc("cadar", 1, 1, function(ar){ return ar[0].car.cdr.car; });
  define_libfunc("caddr", 1, 1, function(ar){ return ar[0].cdr.cdr.car; });
  define_libfunc("cdaar", 1, 1, function(ar){ return ar[0].car.car.cdr; });
  define_libfunc("cdadr", 1, 1, function(ar){ return ar[0].cdr.car.cdr; });
  define_libfunc("cddar", 1, 1, function(ar){ return ar[0].car.cdr.cdr; });
  define_libfunc("cdddr", 1, 1, function(ar){ return ar[0].cdr.cdr.cdr; });
  define_libfunc("caaaar", 1, 1, function(ar){ return ar[0].car.car.car.car; });
  define_libfunc("caaadr", 1, 1, function(ar){ return ar[0].cdr.car.car.car; });
  define_libfunc("caadar", 1, 1, function(ar){ return ar[0].car.cdr.car.car; });
  define_libfunc("caaddr", 1, 1, function(ar){ return ar[0].cdr.cdr.car.car; });
  define_libfunc("cadaar", 1, 1, function(ar){ return ar[0].car.car.cdr.car; });
  define_libfunc("cadadr", 1, 1, function(ar){ return ar[0].cdr.car.cdr.car; });
  define_libfunc("caddar", 1, 1, function(ar){ return ar[0].car.cdr.cdr.car; });
  define_libfunc("cadddr", 1, 1, function(ar){ return ar[0].cdr.cdr.cdr.car; });
  define_libfunc("cdaaar", 1, 1, function(ar){ return ar[0].car.car.car.cdr; });
  define_libfunc("cdaadr", 1, 1, function(ar){ return ar[0].cdr.car.car.cdr; });
  define_libfunc("cdadar", 1, 1, function(ar){ return ar[0].car.cdr.car.cdr; });
  define_libfunc("cdaddr", 1, 1, function(ar){ return ar[0].cdr.cdr.car.cdr; });
  define_libfunc("cddaar", 1, 1, function(ar){ return ar[0].car.car.cdr.cdr; });
  define_libfunc("cddadr", 1, 1, function(ar){ return ar[0].cdr.car.cdr.cdr; });
  define_libfunc("cdddar", 1, 1, function(ar){ return ar[0].car.cdr.cdr.cdr; });
  define_libfunc("cddddr", 1, 1, function(ar){ return ar[0].cdr.cdr.cdr.cdr; });

  define_libfunc("null?", 1, 1, function(ar){
    return (ar[0] === nil);
  });
  define_libfunc("list?", 1, 1, function(ar){
    var contents = [];
    for(var o=ar[0]; o != nil; o=o.cdr){
      if(o == nil) return true;
      if(!(o instanceof Pair)) return false;
      if(_.detect(contents, function(item){ return item === o.car; }))
        return false; //cyclic
      contents.push(o.car);
    }
    return true;
  });
  define_libfunc("list", 0, null, function(ar){
    var l = nil;
    for(var i=ar.length-1; i>=0; i--)
      l = new Pair(ar[i], l);
    return l;
  });
  define_libfunc("length", 1, 1, function(ar){
    assert_list(ar[0]);
    var n = 0;
    for(var o=ar[0]; o!=nil; o=o.cdr)
      n++;
    return n;
  });
  define_libfunc("append", 2, null, function(ar){
    var k = ar.length
    var ret = ar[--k];
    while(k--){
      _.each(ar[k].to_array().reverse(), function(item){
        ret = new Pair(item, ret);
      });
    }
    return ret;
  });
  define_libfunc("reverse", 1, 1, function(ar){
    if(!ar[0] instanceof Pair) throw new Error("reverse needs pair but got " + ar[0]);

    var l = nil;
    for(var o=ar[0]; o!=nil; o=o.cdr)
      l = new Pair(o.car, l);
    return l;
  });
  define_libfunc("list-tail", 2, 2, function(ar){
    if(!ar[0] instanceof Pair) throw new Error("list-tail needs pair but got " + ar[0]);

    var o = ar[0];
    for(var i=0; i<ar[1]; i++){
      if(!o instanceof Pair) throw new Error("list-tail: the list is shorter than " + ar[1]);
      o = o.cdr;
    }
    return o;
  });
  define_libfunc("list-ref", 2, 2, function(ar){
    if(!ar[0] instanceof Pair) throw new Error("list-ref needs pair but got " + ar[0]);

    var o = ar[0];
    for(var i=0; i<ar[1]; i++){
      if(!o instanceof Pair) throw new Error("list-ref: the list is shorter than " + ar[1]);
      o = o.cdr;
    }
    return o.car;
  });
  define_libfunc("map", 2, null, function(ar){
    var proc = ar.shift(), lists = ar;
    _.each(lists, assert_list);

    var a = [];
    return Call.multi_foreach(lists, {
      // Called for each element
      // input: the element (or the elements, if more than one list is given)
      // output: a Call request of proc and args
      call: function(xs){ 
        return new Call(proc, _.map(xs, function(x){ return x.car }));
      },

      // Called when each Call request is finished
      // input: the result of Call request,
      //   the element(s) of the Call request (which is not used here)
      // output: `undefined' to continue,
      //   some value to terminate (the value will be the result)
      result: function(res){ a.push(res); },

      // Called when reached to the end of the list(s)
      // input: none
      // output: the resultant value 
      finish: function(){ return array_to_list(a); }
    })
  })
  define_libfunc("for-each", 2, null, function(ar){
    var proc = ar.shift(), lists = ar;
    _.each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function(xs){ 
        return new Call(proc, _.map(xs, function(x){ return x.car }));
      },
      finish: function(){ return BiwaScheme.undef; }
    })
  })

  //        11.10  Symbols

  define_libfunc("symbol?", 1, 1, function(ar){
    return (ar[0] instanceof Symbol) ? true : false;
  });
  define_libfunc("symbol->string", 1, 1, function(ar){
    assert_symbol(ar[0]);
    return ar[0].name;
  });
  define_libfunc("symbol=?", 2, null, function(ar){
    assert_symbol(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_symbol(ar[i]);
      if(ar[i] != ar[0]) return false;
    }
    return true;
  });
  define_libfunc("string->symbol", 1, 1, function(ar){
    assert_string(ar[0]);
    return Sym(ar[0]);
  });

  //
  //        11.11  Characters
  //
  define_libfunc('char?', 1, 1, function(ar){
    return (ar[0] instanceof Char);
  });
  define_libfunc('char->integer', 1, 1, function(ar){
    assert_char(ar[0]);
    return ar[0].value.charCodeAt(0);
  })
  define_libfunc('integer->char', 1, 1, function(ar){
    assert_integer(ar[0]);
    return Char.get(String.fromCharCode(ar[0]));
  })

  var make_char_compare_func = function(test){
    return function(ar){
      assert_char(ar[0]);
      for(var i=1; i<ar.length; i++){
        assert_char(ar[i]);
        if(!test(ar[i-1].value, ar[i].value))
          return false;
      }
      return true;
    }
  }
  define_libfunc('char=?', 2, null, 
    make_char_compare_func(function(a, b){ return a == b }))
  define_libfunc('char<?', 2, null, 
    make_char_compare_func(function(a, b){ return a < b }))
  define_libfunc('char>?', 2, null, 
    make_char_compare_func(function(a, b){ return a > b }))
  define_libfunc('char<=?', 2, null, 
    make_char_compare_func(function(a, b){ return a <= b }))
  define_libfunc('char>=?', 2, null, 
    make_char_compare_func(function(a, b){ return a >= b }))

  //
  //        11.12  Strings
  //
  define_libfunc("string?", 1, 1, function(ar){
    return (typeof(ar[0]) == "string"); 
  })
  define_libfunc("make-string", 1, 2, function(ar){
    assert_integer(ar[0]);
    var c = " ";
    if(ar[1]){
      assert_char(ar[1]);
      c = ar[1].value;
    }
    var out = "";
    _.times(ar[0], function() { out += c; });
    return out;
  })
  define_libfunc("string", 1, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_char(ar[i]);
    return _.map(ar, function(c){ return c.value }).join("");
  })
  define_libfunc("string-length", 1, 1, function(ar){
    assert_string(ar[0]);
    return ar[0].length;
  })
  define_libfunc("string-ref", 2, 2, function(ar){
    assert_string(ar[0]);
    assert_between(ar[1], 0, ar[0].length-1);
    return Char.get(ar[0].charAt([ar[1]]));
  })
  define_libfunc("string=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(ar[0] != ar[i]) return false;
    }
    return true;
  })
  define_libfunc("string<?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] < ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string>?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] > ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string<=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] <= ar[i])) return false;
    }
    return true;
  })
  define_libfunc("string>=?", 2, null, function(ar){
    assert_string(ar[0]);
    for(var i=1; i<ar.length; i++){
      assert_string(ar[i]);
      if(!(ar[i-1] >= ar[i])) return false;
    }
    return true;
  })

  define_libfunc("substring", 3, 3, function(ar){
    assert_string(ar[0]);
    assert_integer(ar[1]);
    assert_integer(ar[2]);

    if(ar[1] < 0) throw new Error("substring: start too small: "+ar[1]);
    if(ar[2] < 0) throw new Error("substring: end too small: "+ar[2]);
    if(ar[0].length+1 <= ar[1]) throw new Error("substring: start too big: "+ar[1]);
    if(ar[0].length+1 <= ar[2]) throw new Error("substring: end too big: "+ar[2]);
    if(!(ar[1] <= ar[2])) throw new Error("substring: not start <= end: "+ar[1]+", "+ar[2]);
    
    return ar[0].substring(ar[1], ar[2]);
  })

  define_libfunc("string-append", 0, null, function(ar){
    for(var i=0; i<ar.length; i++)
      assert_string(ar[i]);
    
    return ar.join("");
  })
  define_libfunc("string->list", 1, 1, function(ar){
    assert_string(ar[0]);
    return array_to_list(_.map(ar[0].split(""), function(s){ return Char.get(s[0]); }));
  })
  define_libfunc("list->string", 1, 1, function(ar){
    assert_list(ar[0]);
    return _.map(ar[0].to_array(), function(c){ return c.value; }).join("");
  })
  define_libfunc("string-for-each", 2, null, function(ar){
    var proc = ar.shift(), strs = ar;
    _.each(strs, assert_string);
    
    return Call.multi_foreach(strs, {
      call: function(chars){ return new Call(proc, chars); },
      finish: function(){ return BiwaScheme.undef; }
    })
  })
  define_libfunc("string-copy", 1, 1, function(ar){
    // note: this is useless, because javascript strings are immutable
    assert_string(ar[0]);
    return ar[0];
  })


  //
  //        11.13  Vectors
  //
  define_libfunc("vector?", 1, 1, function(ar){
    return (ar[0] instanceof Array) && (ar[0].closure_p !== true)
  })
  define_libfunc("make-vector", 1, 2, function(ar){
    assert_integer(ar[0]);
    var vec = new Array(ar[0]);

    if(ar.length == 2){
      for(var i=0; i<ar[0]; i++)
        vec[i] = ar[1];
    }
    return vec;
  })
  define_libfunc("vector", 1, null, function(ar){
    return ar;
  })
  define_libfunc("vector-length", 1, 1, function(ar){
    assert_vector(ar[0]);
    return ar[0].length;
  })
  define_libfunc("vector-ref", 2, 2, function(ar){
    assert_vector(ar[0]);
    assert_integer(ar[1]);

    return ar[0][ar[1]];
  })
  define_libfunc("vector-set!", 3, 3, function(ar){
    assert_vector(ar[0]);
    assert_integer(ar[1]);

    ar[0][ar[1]] = ar[2];
    return BiwaScheme.undef;
  })
  define_libfunc("vector->list", 1, 1, function(ar){
    assert_vector(ar[0]);
    return array_to_list(ar[0]);
  })
  define_libfunc("list->vector", 1, 1, function(ar){
    assert_list(ar[0]);
    return ar[0].to_array();
  })
  define_libfunc("vector-fill!", 2, 2, function(ar){
    assert_vector(ar[0]);
    var vec = ar[0], obj = ar[1];

    for(var i=0; i<vec.length; i++)
      vec[i] = obj;
    return vec;
  })
  define_libfunc("vector-map", 2, null, function(ar){
    var proc = ar.shift(), vecs = ar;
    _.each(vecs, assert_vector);

    var a = [];
    return Call.multi_foreach(vecs, {
      call: function(objs){ return new Call(proc, objs); },
      result: function(res){ a.push(res); },
      finish: function(){ return a; }
    })
  })
  define_libfunc("vector-for-each", 2, null, function(ar){
    var proc = ar.shift(), vecs = ar;
    _.each(vecs, assert_vector);

    return Call.multi_foreach(vecs, {
      call: function(objs){ return new Call(proc, objs); },
      finish: function(){ return BiwaScheme.undef; }
    })
  })

  //
  //        11.14  Errors and violations
  //
//(error who message irritant1 ...)    procedure 
//(assertion-violation who message irritant1 ...)    procedure 
//(assert <expression>)    syntax 
  
  //
  //        11.15  Control features
  //
  define_libfunc("apply", 2, null, function(ar){
    var proc = ar.shift(), rest_args = ar.pop(), args = ar;
    args = args.concat(rest_args.to_array());

    return new Call(proc, args);
  })
  define_syntax("call-with-current-continuation", function(x){
    return new Pair(Sym("call/cc"),
             x.cdr);
  })
  define_libfunc("values", 0, null, function(ar){
    return new Values(ar);
  })
  define_libfunc("call-with-values", 2, 2, function(ar){
    var producer = ar[0], consumer = ar[1];
    return new Call(producer, [], function(ar){
      var values = ar[0];
      if(!(values instanceof Values))
        throw new Error("values expected, but got "+to_write(values));

      return new Call(consumer, values.content);
    })
  })

  //
  //dynamic-wind
  
  //        11.16  Iteration
  //named let
  
  //        11.17  Quasiquotation
  //quasiquote
  var expand_qq = function(f, lv){
    if(f instanceof Symbol || f === nil){
      return List(Sym("quote"), f);
    }
    else if(f instanceof Pair){
      var car = f.car;
      if(car instanceof Pair && car.car === Sym("unquote-splicing")){
        var lv = lv-1;
        if(lv == 0)
          return List(Sym("append"),
                      f.car.cdr.car,
                      expand_qq(f.cdr, lv+1));
        else
          return List(Sym("cons"),
                      List(Sym("list"), Sym("unquote-splicing"), expand_qq(f.car.cdr.car, lv)),
                      expand_qq(f.cdr, lv+1));
      }
      else if(car === Sym("unquote")){
        var lv = lv-1;
        if(lv == 0)
          return f.cdr.car;
        else
          return List(Sym("list"),
                      List(Sym("quote"), Sym("unquote")),
                      expand_qq(f.cdr.car, lv));
      }
      else if(car === Sym("quasiquote"))
        return List(Sym("list"),
                    Sym("quasiquote"),
                    expand_qq(f.cdr.car, lv+1));
      else
        return List(Sym("cons"),
                    expand_qq(f.car, lv),
                    expand_qq(f.cdr, lv));
    }
    else if(f instanceof Array){
      throw new Bug("vector quasiquotation is not implemented yet");
    }
//      // `#(1 2 (unquote f))
//      // (vector 1 2 f)
//      // `#(1 2 (unquote-splicing f) 3)
//      // (vector-append
//      //   (vector 1 2)
//      //   f
//      //   (vector 3))
//      // `#(1 2 `#(3 ,,f) 4)
//      // (vector 1 2 `#(3 ,g) 4)
//      var len = f.length;
//      if(len == 0) return f;
//
//      var vecs = [[]];
//      for(var i=0; i<len; i++){
//        if(f[i] instanceof Pair){
//          if(f[i].car === Sym("unquote")){
//            var lv = lv - 1;
//            if(lv == 0)
//              vecs.last().push(f[i]);
//            else
//              vecs.push()
//          }
//      }
//
//      var car = f[0];
//      if(car === Sym("unquote")){
//        var lv = lv - 1;
//        if(lv == 0)
//          return f.cdr.car;
//        else
//          return List(Sym("vector"),
//                      List(Sym("quote"), Sym("unquote")),
//                      expand_qq(f.cdr.car, lv));
//      }
//      else{
////        return [ Sym("vector"),
////                 expand_qq(
//      }
//    }
//  }
    else
      return f;
  }
  define_syntax("quasiquote", function(x){
    return expand_qq(x.cdr.car, 1);
  })
  //unquote
  define_syntax("unquote", function(x){
    throw new Error("unquote(,) must be inside quasiquote(`)");
  })
  //unquote-splicing
  define_syntax("unquote-splicing", function(x){
    throw new Error("unquote-splicing(,@) must be inside quasiquote(`)");
  })
  
  //        11.18  Binding constructs for syntactic keywords
  //let-syntax
  //letrec-syntax
  
  //        11.19  Macro transformers
  //syntax-rules
  //identifier-syntax
  //
  
  //        11.20  Tail calls and tail contexts
  //(no library function introduced)


  ///
  /// R6RS Standard Libraries
  ///

  //
  // Chapter 1 Unicode
  //
//(char-upcase char)    procedure 
//(char-downcase char)    procedure 
//(char-titlecase char)    procedure 
//(char-foldcase char)    procedure 
//
//(char-ci=? char1 char2 char3 ...)    procedure 
//(char-ci<? char1 char2 char3 ...)    procedure 
//(char-ci>? char1 char2 char3 ...)    procedure 
//(char-ci<=? char1 char2 char3 ...)    procedure 
//(char-ci>=? char1 char2 char3 ...)    procedure 
//
//(char-alphabetic? char)    procedure 
//(char-numeric? char)    procedure 
//(char-whitespace? char)    procedure 
//(char-upper-case? char)    procedure 
//(char-lower-case? char)    procedure 
//(char-title-case? char)    procedure 
//
//(char-general-category char)    procedure 

  //(string-upcase string)    procedure 
  define_libfunc("string-upcase", 1, 1, function(ar){
    assert_string(ar[0]);
    return ar[0].toUpperCase();
  });
  //(string-downcase string)    procedure 
  define_libfunc("string-downcase", 1, 1, function(ar){
    assert_string(ar[0]);
    return ar[0].toLowerCase();
  });
//(string-titlecase string)    procedure 
//(string-foldcase string)    procedure

  BiwaScheme.make_string_ci_function = function(compare){
    return function(ar){
      assert_string(ar[0]);
      var str = ar[0].toUpperCase();

      for(var i=1; i<ar.length; i++){
        assert_string(ar[i]);
        if (!compare(str, ar[i].toUpperCase()))
          return false;
      }
      return true;
    }
  };
  //(string-ci=? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci=?", 2, null, 
    make_string_ci_function(function(a, b){ return a == b; }));
  //(string-ci<? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci<?", 2, null, 
    make_string_ci_function(function(a, b){ return a < b; }));
  //(string-ci>? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci>?", 2, null, 
    make_string_ci_function(function(a, b){ return a > b; }));
  //(string-ci<=? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci<=?", 2, null, 
    make_string_ci_function(function(a, b){ return a <= b; }));
  //(string-ci>=? string1 string2 string3 ...)    procedure 
  define_libfunc("string-ci>=?", 2, null, 
    make_string_ci_function(function(a, b){ return a >= b; }));

//(string-normalize-nfd string)    procedure 
//(string-normalize-nfkd string)    procedure 
//(string-normalize-nfc string)    procedure 
//(string-normalize-nfkc string)    procedure 

  //
  // Chapter 2 Bytevectors
  //

  //
  // Chapter 3 List utilities
  //
  define_libfunc("find", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) return x.car; },
      finish: function(){ return false }
    })
  })
  define_libfunc("for-all", 2, null, function(ar){
    var proc = ar.shift();
    var lists = ar;
    _.each(lists, assert_list);

    var last = true; //holds last result which proc returns
    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        return new Call(proc, _.map(pairs, function(x){ return x.car }));
      },
      result: function(res, pairs){ 
        if(res === false) return false;
        last = res;
      },
      finish: function(){ return last; }
    })
  })
  define_libfunc("exists", 2, null, function(ar){
    var proc = ar.shift();
    var lists = ar;
    _.each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        return new Call(proc, _.map(pairs, function(x){ return x.car }));
      },
      result: function(res, pairs){ 
        if(res !== false) return res;
      },
      finish: function(){ return false; }
    })
  })
  define_libfunc("filter", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);

    var a = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) a.push(x.car); },
      finish: function(){ return array_to_list(a) }
    })
  })
//  define_scmfunc("partition+", 2, 2, 
//    "(lambda (proc ls)  \
//       (define (partition2 proc ls t f) \
//         (if (null? ls) \
//           (values (reverse t) (reverse f)) \
//           (if (proc (car ls)) \
//             (partition2 proc (cdr ls) (cons (car ls) t) f) \
//             (partition2 proc (cdr ls) t (cons (car ls) f))))) \
//       (partition2 proc ls '() '()))");

  define_libfunc("partition", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);

    var t = [], f = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ 
        if(res) t.push(x.car); 
        else    f.push(x.car); 
      },
      finish: function(){ 
        return new Values([array_to_list(t), array_to_list(f)]);
      }
    })
  })
  define_libfunc("fold-left", 3, null, function(ar){
    var proc = ar.shift(), accum = ar.shift(), lists = ar;
    _.each(lists, assert_list);

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        var args = _.map(pairs, function(x){ return x.car });
        args.unshift(accum);
        return new Call(proc, args);
      },
      result: function(res, pairs){ accum = res; },
      finish: function(){ return accum; }
    })
  })
  define_libfunc("fold-right", 3, null, function(ar){
    var proc = ar.shift(), accum = ar.shift();
    var lists = _.map(ar, function(ls){
      // reverse each list
      assert_list(ls);
      return array_to_list(ls.to_array().reverse());
    })

    return Call.multi_foreach(lists, {
      call: function(pairs){ 
        var args = _.map(pairs, function(x){ return x.car });
        args.push(accum);
        return new Call(proc, args);
      },
      result: function(res, pairs){ accum = res; },
      finish: function(){ return accum; }
    })
  })
  define_libfunc("remp", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);

    var ret = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(!res) ret.push(x.car); },
      finish: function(){ return array_to_list(ret); }
    })
  })
  var make_remover = function(key){
    return function(ar){ 
      var obj = ar[0], ls = ar[1];
      assert_list(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function(x){ 
          return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car]) 
        },
        result: function(res, x){ if(!res) ret.push(x.car); },
        finish: function(){ return array_to_list(ret); }
      })
    }
  }
  define_libfunc("remove", 2, 2, make_remover("equal?"));
  define_libfunc("remv", 2, 2, make_remover("eqv?"));
  define_libfunc("remq", 2, 2, make_remover("eq?"));

  define_libfunc("memp", 2, 2, function(ar){
    var proc = ar[0], ls = ar[1];
    assert_list(ls);

    var ret = [];
    return Call.foreach(ls, {
      call: function(x){ return new Call(proc, [x.car]) },
      result: function(res, x){ if(res) return x; },
      finish: function(){ return false; }
    })
  })
  var make_finder = function(key){
    return function(ar){ 
      var obj = ar[0], ls = ar[1];
      assert_list(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function(x){ 
          return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car]) 
        },
        result: function(res, x){ if(res) return x; },
        finish: function(){ return false; }
      })
    }
  }
  define_libfunc("member", 2, 2, make_finder("equal?"));
  define_libfunc("memv", 2, 2, make_finder("eqv?"));
  define_libfunc("memq", 2, 2, make_finder("eq?"));
  
  define_libfunc("assp", 2, 2, function(ar){
    var proc = ar[0], als = ar[1];
    assert_list(als);

    var ret = [];
    return Call.foreach(als, {
      call: function(x){ 
        if(x.car.car)
          return new Call(proc, [x.car.car]);
        else
          throw new Error("ass*: pair required but got "+to_write(x.car));
      },
      result: function(res, x){ if(res) return x.car; },
      finish: function(){ return false; }
    })
  })
  var make_assoc = function(key){
    return function(ar){ 
      var obj = ar[0], ls = ar[1];
      assert_list(ls);

      var ret = [];
      return Call.foreach(ls, {
        call: function(x){ 
          if(x.car.car)
            return new Call(TopEnv[key] || CoreEnv[key], [obj, x.car.car]) 
          else
            throw new Error("ass*: pair required but got "+to_write(x.car));
        },
        result: function(res, x){ if(res) return x.car; },
        finish: function(){ return false; }
      })
    }
  }
  define_libfunc("assoc", 2, 2, make_assoc("equal?"));
  define_libfunc("assv", 2, 2, make_assoc("eqv?"));
  define_libfunc("assq", 2, 2, make_assoc("eq?"));

  define_libfunc("cons*", 1, null, function(ar){
    if(ar.length == 1)
      return ar[0];
    else{
      var ret = null;
      _.each(ar.reverse(), function(x){
        if(ret){
          ret = new Pair(x, ret);
        }
        else
          ret = x;
      })
      return ret;
    }
  })

  //
  // Chapter 4 Sorting
  //
  // NOTE: compare function is not supported yet.
  // it is partially implemented as list-sort/comp
  // (see extra_lib.js).
  // TODO: implement some sorting algorithm in CPS
  // so that setTimeout in compare procedures work well
  //
  //(list-sort list)
  define_libfunc("list-sort", 1, 2, function(ar){
    if(ar[1]){
      throw new Bug("list-sort: cannot take compare proc now");
    }
    assert_list(ar[0]);
    return array_to_list(ar[0].to_array().sort());
  });

  //(vector-sort proc vector)    procedure
  define_libfunc("vector-sort", 1, 2, function(ar){
    if(ar[1]){
      throw new Bug("vector-sort: cannot take compare proc now");
    }
    assert_vector(ar[0]);
      return _.clone(ar[0]).sort();
  });

  //(vector-sort! proc vector)    procedure 
  define_libfunc("vector-sort!", 1, 2, function(ar){
    if(ar[1]){
      throw new Bug("vector-sort!: cannot take compare proc now");
    }
    assert_vector(ar[0]);
    ar[0].sort();
    return BiwaScheme.undef;
  });

  //
  // Chapter 5 Control Structures
  //
  define_syntax("when", function(x){
    //(when test body ...) 
    //=> (if test (begin body ...) #<undef>)
    var test = x.cdr.car, body = x.cdr.cdr;

    return new Pair(Sym("if"), 
             new Pair(test,
               new Pair(new Pair(Sym("begin"), body),
                 new Pair(BiwaScheme.undef, nil))));
  });

  define_syntax("unless", function(x){
    //(unless test body ...) 
    //=> (if (not test) (begin body ...) #<undef>)
    var test = x.cdr.car, body = x.cdr.cdr;

    return new Pair(Sym("if"), 
             new Pair(new Pair(Sym("not"), new Pair(test, nil)),
               new Pair(new Pair(Sym("begin"), body),
                 new Pair(BiwaScheme.undef, nil))));
  });

  define_syntax("do", function(x){
    //(do ((var1 init1 step1)
    //     (var2 init2 step2) ...)
    //    (test expr1 expr2 ...)
    //  body1 body2 ...)
    //=> (let loop` ((var1 init1) (var2 init2) ...)
    //     (if test 
    //       (begin expr1 expr2 ...)
    //       (begin body1 body2 ...
    //              (loop` step1 step2 ...)))))

    // parse arguments
    if(!BiwaScheme.isPair(x.cdr))
      throw new Error("do: no variables of do");
    var varsc = x.cdr.car;
    if(!BiwaScheme.isPair(varsc))
      throw new Error("do: variables must be given as a list");
    if(!BiwaScheme.isPair(x.cdr.cdr))
      throw new Error("do: no resulting form of do");
    var resultc = x.cdr.cdr.car;
    var bodyc = x.cdr.cdr.cdr;

    // construct subforms
    var loop = BiwaScheme.gensym();

    var init_vars = array_to_list(varsc.map(function(var_def){
      var a = var_def.to_array();
      return List(a[0], a[1]);
    }));

    var test = resultc.car;
    var result_exprs = new Pair(Sym("begin"), resultc.cdr);

    var next_loop = new Pair(loop, array_to_list(varsc.map(function(var_def){
      var a = var_def.to_array();
      return a[2] || a[0];
    })));
    var body_exprs = new Pair(Sym("begin"), bodyc).concat(List(next_loop));

    // combine subforms 
    return List(Sym("let"), 
                loop,
                init_vars,
                List(Sym("if"),
                     test,
                     result_exprs,
                     body_exprs));
  });

  //(case-lambda <case-lambda clause> ...)    syntax
  define_syntax("case-lambda", function(x){
    // (case-lambda (() body0 ...)
    //              ((a) body1 ...)
    //              ((a b . cc) body2 ...)
    //              (rest bodyn ...))
    //=> (lambda args`
    //     (let1 len` (length args`)
    //       (if (= len` (length '()))
    //         ((lambda () body0 ...) args`)
    //         (if (= len` (length '(a)))
    //           ((lambda (a) body1 ...) args`)
    //           (if (>= len` (length '(a b)))
    //             ((lambda (a b . cc) body2 ...) args`)
    //             ((lambda rest bodyn ...) args`)
//    var len = BiwaScheme.gensym();
//    if(!BiwaScheme.isPair(x.cdr))
//      throw new Error("do: no variables of do");

  });

  //
  // Chapter 6 Records
  //
  // 6.2 Records: Syntactic layer
//eqv, eq
//(define-record-type <name spec> <record clause>*)    syntax 
  define_syntax("define-record-type", function(x){
    // (define-record-type <name spec> <record clause>*)
    var name_spec = x.cdr.car;
    var record_clauses = x.cdr.cdr;

    // 1. parse name spec
    // <name spec>: either
    // - <record name> eg: point
    // - (<record name> <constructor name> <predicate name>) 
    //   eg: (point make-point point?)
    if(BiwaScheme.isSymbol(name_spec)){
      var record_name = name_spec;
      var constructor_name = Sym("make-"+name_spec.name);
      var predicate_name = Sym(name_spec.name+"?");
    }
    else{
      assert_list(name_spec);
      var record_name = name_spec.car;
      var constructor_name = name_spec.cdr.car;
      var predicate_name = name_spec.cdr.cdr.car;
      assert_symbol(record_name);
      assert_symbol(constructor_name);
      assert_symbol(predicate_name);
    }
    
    // 2. parse record clauses
    var sealed = false;
    var opaque = false;
    var nongenerative = false;
    var uid = false;
    var parent_name;
    var parent_rtd = false;
    var parent_cd = false;
    var protocol = false;
    var fields = [];

    // <record clause>:
    _.each(record_clauses.to_array(), function(clause){
      switch(clause.car){
        // - (fields <field spec>*)
        case Sym("fields"):
          fields = _.map(clause.cdr.to_array(), function(field_spec, idx){
            if(BiwaScheme.isSymbol(field_spec)){
              // - <field name>
              return {name: field_spec, idx: idx, mutable: false,
                      accessor_name: null, mutator_name: null};
            }
            else{
              assert_list(field_spec);
              assert_symbol(field_spec.car);
              switch(field_spec.car){
                case Sym("immutable"):
                  // - (immutable <field name>)
                  // - (immutable <field name> <accessor name>)
                  var field_name = field_spec.cdr.car;
                  assert_symbol(field_name);

                  if(BiwaScheme.isNil(field_spec.cdr.cdr))
                    return {name: field_name, idx: idx, mutable: false};
                  else
                    return {name: field_name, idx: idx, mutable: false,
                            accessor_name: field_spec.cdr.cdr.car};
                  break;

                case Sym("mutable"):
                  // - (mutable <field name>)
                  // - (mutable <field name> <accessor name> <mutator name>)
                  var field_name = field_spec.cdr.car;
                  assert_symbol(field_name);

                  if(BiwaScheme.isNil(field_spec.cdr.cdr))
                    return {name: field_name, idx: idx, mutable: true}
                  else
                    return {name: field_name, idx: idx, mutable: true,
                            accessor_name: field_spec.cdr.cdr.car,
                            mutator_name: field_spec.cdr.cdr.cdr.car};
                  break;
                default:
                  throw new Error("define-record-type: field definition "+
                              "must start with `immutable' or 'mutable'");
              }
            }
          });
          break;
        // - (parent <name>)
        case Sym("parent"):
          parent_name = clause.cdr.car;
          assert_symbol(parent_name);
          break;
        // - (protocol <expr>)
        case Sym("protocol"):
          protocol = clause.cdr.car;
          break;
        // - (sealed <bool>)
        case Sym("sealed"):
          sealed = !!clause.cdr.car;
          break;
        // - (opaque <bool>)
        case Sym("opaque"):
          opaque = !!clause.cdr.car;
          break;
        // - (nongenerative <uid>?)
        case Sym("nongenerative"):
          nongenerative = true;
          uid = clause.cdr.car;
          break;
        // - (parent-rtd <rtd> <cd>)
        case Sym("parent-rtd"):
          parent_rtd = clause.cdr.car;
          parent_cd = clause.cdr.cdr.car;
          break;
        default:
          throw new BiwaScheme.Error("define-record-type: unknown clause `"+
                                     BiwaScheme.to_write(clause.car)+"'");
      }
    });

    if(parent_name){
      parent_rtd = [Sym("record-type-descriptor"), parent_name];
      parent_cd  = [Sym("record-constructor-descriptor"), parent_name];
    }

    // 3. build the definitions
    // Note: In this implementation, rtd and cd are not bound to symbols.
    // They are referenced through record name by record-type-descriptor
    // and record-constructor-descriptor. These relation are stored in
    // the hash BiwaScheme.Record._DefinedTypes.
    var rtd = [Sym("record-type-descriptor"), record_name];
    var cd  = [Sym("record-constructor-descriptor"), record_name];

    // registration
    var rtd_fields = _.map(fields, function(field){
      return List(Sym(field.mutable ? "mutable" : "immutable"), field.name);
    });
    rtd_fields.is_vector = true; //tell List not to convert
    var rtd_def = [Sym("make-record-type-descriptor"),
                    [Sym("quote"), record_name], parent_rtd, uid,
                    sealed, opaque, rtd_fields];
    var cd_def = [Sym("make-record-constructor-descriptor"),
                    Sym("__rtd"), parent_cd, protocol];
    var registration =
      [Sym("let*"), [[Sym("__rtd"), rtd_def],
                    [Sym("__cd"), cd_def]],
        [Sym("_define-record-type"),
          [Sym("quote"), record_name], Sym("__rtd"), Sym("__cd")]];

    // accessors and mutators
    var accessor_defs = _.map(fields, function(field){
      var name = field.accessor_name ||
                   Sym(record_name.name+"-"+field.name.name);

      return [Sym("define"), name, [Sym("record-accessor"), rtd, field.idx]];
    });

    var mutator_defs = _.filter(fields, function(field){
      return field.mutable;
    });
    mutator_defs = _.map(mutator_defs, function(field){
      var name = field.mutator_name ||
                   Sym("set-"+record_name.name+"-"+field.name.name+"!");

      return [Sym("define"), name, [Sym("record-mutator"), rtd, field.idx]];
    });

    // wrap the definitions with `begin'
    return List.apply(null,
      [Sym("begin"),
        registration,
        [Sym("define"), constructor_name, [Sym("record-constructor"), cd]],
        [Sym("define"), predicate_name, [Sym("record-predicate"), rtd]],
        ].concat(accessor_defs).
          concat(mutator_defs)
    );
  });
  define_libfunc("_define-record-type", 3, 3, function(ar){
    assert_symbol(ar[0]);
    assert_record_td(ar[1]);
    assert_record_cd(ar[2]);
    BiwaScheme.Record.define_type(ar[0].name, ar[1], ar[2]);
    return BiwaScheme.undef;
  });
//(record-type-descriptor <record name>)    syntax 
  define_syntax("record-type-descriptor", function(x){
    return List(Sym("_record-type-descriptor"), [Sym("quote"), x.cdr.car]);
  });
  define_libfunc("_record-type-descriptor", 1, 1, function(ar){
    assert_symbol(ar[0]);
    var type = BiwaScheme.Record.get_type(ar[0].name);
    if(type)
      return type.rtd;
    else
      throw new Error("record-type-descriptor: unknown record type "+ar[0].name);
  });
//(record-constructor-descriptor <record name>)    syntax 
  define_syntax("record-constructor-descriptor", function(x){
    return List(Sym("_record-constructor-descriptor"), [Sym("quote"), x.cdr.car]);
  });
  define_libfunc("_record-constructor-descriptor", 1, 1, function(ar){
    assert_symbol(ar[0]);
    var type = BiwaScheme.Record.get_type(ar[0].name);
    if(type)
      return type.cd;
    else
      throw new Error("record-constructor-descriptor: unknown record type "+ar[0].name);
  });

  // 6.3  Records: Procedural layer
//(make-record-type-descriptor name    procedure
  define_libfunc("make-record-type-descriptor", 6, 6, function(ar){
    var name = ar[0], parent_rtd = ar[1], uid = ar[2],
        sealed = ar[3], opaque = ar[4], fields = ar[5];
    
    assert_symbol(name);
    if(parent_rtd) assert_record_td(parent_rtd);
    if(uid){
      assert_symbol(uid);
      var _rtd = BiwaScheme.Record.RTD.NongenerativeRecords[uid.name];
      if(_rtd){
        // the record type is already defined.
        return _rtd;
        // should check equality of other arguments..
      }
    }
    sealed = !!sealed;
    opaque = !!opaque;
    assert_vector(fields);
    for(var i=0; i<fields.length; i++){
      var list = fields[i];
      assert_symbol(list.car, "mutability");
      assert_symbol(list.cdr.car, "field name");
      fields[i] = [list.cdr.car.name, (list.car == Sym("mutable"))];
    };

    var rtd = new BiwaScheme.Record.RTD(name, parent_rtd, uid,
                                     sealed, opaque, fields);
    if(uid)
      BiwaScheme.Record.RTD.NongenerativeRecords[uid.name] = rtd;

    return rtd;
  });
//(record-type-descriptor? obj)    procedure 
  define_libfunc("record-type-descriptor?", 1, 1, function(ar){
    return (ar[0] instanceof BiwaScheme.Record.RTD);
  });
//(make-record-constructor-descriptor rtd    procedure 
  define_libfunc("make-record-constructor-descriptor", 3, 3, function(ar){
    var rtd = ar[0], parent_cd = ar[1], protocol = ar[2];

    assert_record_td(rtd);
    if(parent_cd) assert_record_cd(parent_cd);
    if(protocol) assert_procedure(protocol);

    return new BiwaScheme.Record.CD(rtd, parent_cd, protocol);
  });
//(record-constructor constructor-descriptor)    procedure
  define_libfunc("record-constructor", 1, 1, function(ar){
    var cd = ar[0];
    assert_record_cd(cd);

    return cd.record_constructor();
  });
//(record-predicate rtd)    procedure
  define_libfunc("record-predicate", 1, 1, function(ar){
    var rtd = ar[0];
    assert_record_td(rtd);

    return function(args){
      var obj = args[0];

      return (obj instanceof BiwaScheme.Record) &&
             (obj.rtd === rtd);
    };
  });
//(record-accessor rtd k)    procedure 
  define_libfunc("record-accessor", 2, 2, function(ar){
    var rtd = ar[0], k = ar[1];
    assert_record_td(rtd);
    assert_integer(k);
    for(var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return function(args){
      var record = args[0];
      assert_record(record);

      var descendant = false;
      for(var _rtd = record.rtd; _rtd; _rtd = _rtd.parent_rtd){
        if(_rtd == rtd) descendant = true;
      }
      assert(descendant,
            "(record-accessor): "+BiwaScheme.to_write(record)+
            " is not a "+rtd.name);

      return record.get(k);
    };
  });
//(record-mutator rtd k)    procedure
  define_libfunc("record-mutator", 2, 2, function(ar){
    var rtd = ar[0], k = ar[1];
    assert_record_td(rtd);
    assert_integer(k);
    for(var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return function(args){
      var record = args[0], val = args[1];
      assert_record(record);
      assert(record.rtd === rtd,
            "(record-mutator): "+BiwaScheme.to_write(record)+
            " is not a "+rtd.name);
      assert(!record.rtd.sealed,
            "(record-mutator): "+rtd.name+" is sealed (can't mutate)");

      record.set(k, val);
    };
  });

  // 6.4  Records: Inspection
//(record? obj)    procedure
  define_libfunc("record?", 1, 1, function(ar){
    var obj = ar[0];
    if(BiwaScheme.isRecord(obj)){
      if(obj.rtd.opaque)
        return false; // opaque records pretend as if it is not a record.
      else
        return true;
    }
    else
      return false;
  });
//(record-rtd record)    procedure
  define_libfunc("record-rtd", 1, 1, function(ar){
    assert_record(ar[0]);
    return ar[0].rtd;
  });
//(record-type-name rtd)    procedure
  define_libfunc("record-type-name", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].name;
  });
//(record-type-parent rtd)    procedure
  define_libfunc("record-type-parent", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].parent_rtd;
  });
//(record-type-uid rtd)    procedure 
  define_libfunc("record-type-uid", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].uid;
  });
//(record-type-generative? rtd)    procedure 
  define_libfunc("record-type-generative?", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].generative;
  });
//(record-type-sealed? rtd)    procedure
  define_libfunc("record-type-sealed?", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].sealed;
  });
//(record-type-opaque? rtd)    procedure
  define_libfunc("record-type-opaque?", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return ar[0].opaque;
  });
//(record-type-field-names rtd)    procedure
  define_libfunc("record-type-field-names", 1, 1, function(ar){
    assert_record_td(ar[0]);
    return _.map(ar[0].fields, function(field){ return field.name; });
  });
//(record-field-mutable? rtd k)    procedure 
  define_libfunc("record-field-mutable?", 2, 2, function(ar){
    var rtd = ar[0], k = ar[1];
    assert_record_td(ar[0]);
    assert_integer(k);

    for(var _rtd = rtd.parent_rtd; _rtd; _rtd = _rtd.parent_rtd)
      k += _rtd.fields.length;

    return ar[0].fields[k].mutable;
  });

  //
  // Chapter 7 Exceptions and conditions
  //
//(with-exception-handler handler thunk)    procedure
//(guard (<variable>    syntax
  //(raise obj)    procedure 
  define_libfunc("raise", 1, 1, function(ar){
    throw new BiwaScheme.UserError(BiwaScheme.to_write(ar[0]));
  });
//(raise-continuable obj)    procedure
//
//&condition    condition type
//(condition condition1 ...)    procedure 
//(simple-conditions condition)    procedure
//(condition? obj)    procedure
//(condition-predicate rtd)    procedure 
//(condition-accessor rtd proc)    procedure
//
//&message    condition type 
//&warning    condition type 
//&serious    condition type 
//&error    condition type 
//&violation    condition type 
//&assertion    condition type 
//&irritants    condition type 
//&who    condition type 
//&non-continuable    condition type 
//&implementation-restriction    condition type 
//&lexical    condition type 
//&syntax    condition type 
//&undefined    condition type 

  //
  // Chapter 8 I/O
  //
//  //    8  I/O
//  //        8.1  Condition types
//&i/o    condition type 
//&i/o-read    condition type 
//&i/o-write    condition type 
//&i/o-invalid-position    condition type 
//&i/o-filename    condition type 
//&i/o-file-protection    condition type
//&i/o-file-is-read-only    condition type
//&i/o-file-already-exists    condition type 
//&i/o-file-does-not-exist    condition type
//&i/o-port    condition type 
//
//  //        8.2  Port I/O
//  //            8.2.1  File names
//  //(no function introduced)
//
//  //            8.2.2  File options
//(file-options <file-options symbol> ...)    syntax 
//
//  //            8.2.3  Buffer modes
//(buffer-mode <buffer-mode symbol>)    syntax  
//(buffer-mode? obj)    procedure
//
//  //            8.2.4  Transcoders
//(latin-1-codec)    procedure 
//(utf-8-codec)    procedure 
//(utf-16-codec)    procedure
//(eol-style <eol-style symbol>)    syntax
//(native-eol-style)    procedure
//&i/o-decoding    condition type
//&i/o-encoding    condition type 
//(error-handling-mode <error-handling-mode symbol>)    syntax 
//(make-transcoder codec)    procedure 
//(make-transcoder codec eol-style)    procedure 
//(make-transcoder codec eol-style handling-mode)    procedure
//(native-transcoder)    procedure
//(transcoder-codec transcoder)    procedure 
//(transcoder-eol-style transcoder)    procedure 
//(transcoder-error-handling-mode transcoder)    procedure 
//(bytevector->string bytevector transcoder)    procedure 
//(string->bytevector string transcoder)    procedure
//
  //            8.2.5  End-of-file object
  //-> 8.3 (eof-object)    procedure 
  //-> 8.3 (eof-object? obj)    procedure 

  //            8.2.6  Input and output ports
  define_libfunc("port?", 1, 1, function(ar){
    return (ar[0] instanceof Port);
  })
//(port-transcoder port)    procedure 
  define_libfunc("textual-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return !ar[0].is_binary;
  })
  define_libfunc("binary-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_binary;
  })
//(transcoded-port binary-port transcoder)    procedure
//(port-has-port-position? port)    procedure 
//(port-position port)    procedure
//(port-has-set-port-position!? port)    procedure 
//(set-port-position! port pos)    procedure
  define_libfunc("close-port", 1, 1, function(ar){
    assert_port(ar[0]);
    ar[0].close();
    return BiwaScheme.undef;
  })
  //(call-with-port port proc)    procedure
  define_libfunc("call-with-port", 2, 2, function(ar){
    var port = ar[0], proc = ar[1];
    assert_port(port);
    assert_closure(proc);

    return new Call(proc, [port], function(ar){
      // Automatically close the port 
      port.close();
      return ar[0]; // TODO: values
    });
  });

  //            8.2.7  Input ports
  //8.3 (input-port? obj)    procedure 
//(port-eof? input-port)    procedure 
//(open-file-input-port filename)    procedure
//(open-bytevector-input-port bytevector)    procedure
//(open-string-input-port string)    procedure 
//(standard-input-port)    procedure 
//8.3 (current-input-port)    procedure
//(make-custom-binary-input-port id read!    procedure
//(make-custom-textual-input-port id read!    procedure
//
//  //            8.2.8  Binary input
//(get-u8 binary-input-port)    procedure
//(lookahead-u8 binary-input-port)    procedure
//(get-bytevector-n binary-input-port count)    procedure 
//(get-bytevector-n! binary-input-port    procedure
//(get-bytevector-some binary-input-port)    procedure
//(get-bytevector-all binary-input-port)    procedure
//
//  //            8.2.9  Textual input
//(get-char textual-input-port)    procedure
//(lookahead-char textual-input-port)    procedure 
//(get-string-n textual-input-port count)    procedure
//(get-string-n! textual-input-port string start count)    procedure
//(get-string-all textual-input-port)    procedure 
//(get-line textual-input-port)    procedure
//(get-datum textual-input-port)    procedure
//
  //            8.2.10  Output ports
  //8.3 (output-port? obj)    procedure
//(flush-output-port output-port)    procedure 
//(output-port-buffer-mode output-port)    procedure 
//(open-file-output-port filename)    procedure 
//(open-bytevector-output-port)    procedure 
//(call-with-bytevector-output-port proc)    procedure 
//(open-string-output-port)    procedure   
//(call-with-string-output-port proc)    procedure 
//(standard-output-port)    procedure 
//(standard-error-port)    procedure 
//8.3 (current-output-port)    procedure 
//8.3 (current-error-port)    procedure 
//(make-custom-binary-output-port id    procedure
  //(make-custom-textual-output-port id write! get-position set-position! close)
//  define_libfunc("make-custom-textual-output-port", 5, 5, function(ar){
//    assert_string(ar[0]);
//    assert_closure(ar[1]);
//    assert_closure(ar[2]);
//    assert_closure(ar[3]);
//    assert_closure(ar[4]);
//    return new Port(ar[0], ar[1], ar[2], ar[3], ar[4]);
//  })
//
//  //            8.2.11  Binary output
//(put-u8 binary-output-port octet)    procedure
//(put-bytevector binary-output-port bytevector)    procedure 
//
  //            8.2.12  Textual output
  define_libfunc("put-char", 2, 2, function(ar){
    assert_port(ar[0]);
    assert_char(ar[1]);
    ar[0].put_string(ar[1].value);
    return BiwaScheme.undef;
  })
  define_libfunc("put-string", 2, 2, function(ar){
    assert_port(ar[0]);
    assert_string(ar[1]);
    ar[0].put_string(ar[1]);
    return BiwaScheme.undef;
  })
  define_libfunc("put-datum", 2, 2, function(ar){
    assert_port(ar[0]);
    ar[0].put_string(to_write(ar[1]));
    return BiwaScheme.undef;
  })
//
//  //            8.2.13  Input/output ports
//(open-file-input/output-port filename)    procedure 
//(make-custom-binary-input/output-port    procedure 
//(make-custom-textual-input/output-port    procedure
//
//  //        8.3  Simple I/O
  define_libfunc("eof-object", 0, 0, function(ar){
    return eof;
  })
  define_libfunc("eof-object?", 1, 1, function(ar){
    return ar[0] === eof;
  })
//(call-with-input-file filename proc)    procedure 
//(call-with-output-file filename proc)    procedure
  define_libfunc("input-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_input;
  })
  define_libfunc("output-port?", 1, 1, function(ar){
    assert_port(ar[0]);
    return ar[0].is_output;
  })
  define_libfunc("current-input-port", 0, 0, function(ar){
    return Port.current_input;
  })
  define_libfunc("current-output-port", 0, 0, function(ar){
    return Port.current_output;
  })
  define_libfunc("current-error-port", 0, 0, function(ar){
    return Port.current_error;
  })
//(with-input-from-file filename thunk)    procedure 
//(with-output-to-file filename thunk)    procedure
//(open-input-file filename)    procedure
//(open-output-file filename)    procedure 
  define_libfunc("close-input-port", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!ar[0].is_input)
      throw new Error("close-input-port: port is not input port");
    ar[0].close();
    return BiwaScheme.undef;
  });
  define_libfunc("close-output-port", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!ar[0].is_output)
      throw new Error("close-output-port: port is not output port");
    ar[0].close();
    return BiwaScheme.undef;
  });
//(read-char)    procedure 
//(peek-char)    procedure 
  define_libfunc("read", 0, 1, function(ar){
    var port = ar[0] || Port.current_input;
    assert_port(port);

    return port.get_string(function(str){
	    return Interpreter.read(str);
    });
  })

  // write-char [1,2]
  define_libfunc("newline", 0, 1, function(ar){
    var port = ar[0] || Port.current_output;
    port.put_string("\n");
    return BiwaScheme.undef;
  });
  define_libfunc("display", 1, 2, function(ar){
    var port = ar[1] || Port.current_output;
    port.put_string(to_display(ar[0]));
    return BiwaScheme.undef;
  });
  define_libfunc("write", 1, 2, function(ar){
    var port = ar[1] || Port.current_output;
    assert_port(port);
    port.put_string(to_write(ar[0]));
    return BiwaScheme.undef;
  });

  //
  // Chapter 9 File System
  //
//(file-exists? filename)    procedure 
  define_libfunc("file-exists?", 1, 1, function(ar){
		netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect"); //TODO: extract to a function
    assert_string(ar[0]);
    var fileIn = FileIO.open(ar[0]);
    return fileIn.exists();
  });
//(delete-file filename)    procedure 
  define_libfunc("delete-file", 1, 1, function(ar){
		netscape.security.PrivilegeManager.enablePrivilege("UniversalXPConnect"); //TODO: extract to a function
    assert_string(ar[0]);
    var deleted = FileIO.unlink(FileIO.open(ar[0]));
    if(!deleted){
      //TODO: raise %i/o-filename if not found or not deletable
      puts("delete-file: cannot delete " + ar[0]);
    }
    return BiwaScheme.undef;
  });

  //
  // Chapter 10 Command-line access and exit values
  //
//(command-line)    procedure
//(exit)    procedure 
//(exit obj)    procedure

  //
  // Chapter 11 Arithmetic
  //
////        11.1  Bitwise operations
////        11.2  Fixnums
//(fixnum? obj)    procedure
//(fixnum-width)    procedure 
//(least-fixnum)    procedure 
//(greatest-fixnum)    procedure 
//(fx=? fx1 fx2 fx3 ...)    procedure 
//(fx>? fx1 fx2 fx3 ...)    procedure 
//(fx<? fx1 fx2 fx3 ...)    procedure 
//(fx>=? fx1 fx2 fx3 ...)    procedure 
//(fx<=? fx1 fx2 fx3 ...)    procedure 
//(fxzero? fx)    procedure 
//(fxpositive? fx)    procedure 
//(fxnegative? fx)    procedure 
//(fxodd? fx)    procedure 
//(fxeven? fx)    procedure
//(fxmax fx1 fx2 ...)    procedure 
//(fxmin fx1 fx2 ...)    procedure
//(fx+ fx1 fx2)    procedure 
//(fx* fx1 fx2)    procedure
//(fx- fx1 fx2)    procedure 
//(fxdiv-and-mod fx1 fx2)    procedure 
//(fxdiv fx1 fx2)    procedure 
//(fxmod fx1 fx2)    procedure 
//(fxdiv0-and-mod0 fx1 fx2)    procedure 
//(fxdiv0 fx1 fx2)    procedure 
//(fxmod0 fx1 fx2)    procedure 
//(fx+/carry fx1 fx2 fx3)    procedure
//(fx-/carry fx1 fx2 fx3)    procedure
//(fx*/carry fx1 fx2 fx3)    procedure 
//(fxnot fx)    procedure
//(fxand fx1 ...)    procedure 
//(fxior fx1 ...)    procedure 
//(fxxor fx1 ...)    procedure
//(fxif fx1 fx2 fx3)    procedure
//(fxbit-count fx)    procedure
//(fxlength fx)    procedure
//(fxfirst-bit-set fx)    procedure 
//(fxbit-set? fx1 fx2)    procedure
//(fxcopy-bit fx1 fx2 fx3)    procedure 
//(fxbit-field fx1 fx2 fx3)    procedure
//(fxcopy-bit-field fx1 fx2 fx3 fx4)    procedure
//(fxarithmetic-shift fx1 fx2)    procedure
//(fxarithmetic-shift-left fx1 fx2)    procedure 
//(fxarithmetic-shift-right fx1 fx2)    procedure
//(fxrotate-bit-field fx1 fx2 fx3 fx4)    procedure
//(fxreverse-bit-field fx1 fx2 fx3)    procedure
//
////        11.3  Flonums
//(flonum? obj)    procedure
//(real->flonum x)    procedure
//(fl=? fl1 fl2 fl3 ...)    procedure 
//(fl<? fl1 fl2 fl3 ...)    procedure 
//(fl<=? fl1 fl2 fl3 ...)    procedure 
//(fl>? fl1 fl2 fl3 ...)    procedure 
//(fl>=? fl1 fl2 fl3 ...)    procedure
//(flinteger? fl)    procedure 
//(flzero? fl)    procedure 
//(flpositive? fl)    procedure 
//(flnegative? fl)    procedure 
//(flodd? ifl)    procedure 
//(fleven? ifl)    procedure 
//(flfinite? fl)    procedure 
//(flinfinite? fl)    procedure 
//(flnan? fl)    procedure
//(flmax fl1 fl2 ...)    procedure 
//(flmin fl1 fl2 ...)    procedure
//(fl+ fl1 ...)    procedure 
//(fl* fl1 ...)    procedure 
//(fl- fl1 fl2 ...)    procedure 
//(fl- fl)    procedure 
//(fl/ fl1 fl2 ...)    procedure 
//(fl/ fl)    procedure 
//(flabs fl)    procedure
//(fldiv-and-mod fl1 fl2)    procedure 
//(fldiv fl1 fl2)    procedure 
//(flmod fl1 fl2)    procedure 
//(fldiv0-and-mod0 fl1 fl2)    procedure 
//(fldiv0 fl1 fl2)    procedure 
//(flmod0 fl1 fl2)    procedure 
//(flnumerator fl)    procedure 
//(fldenominator fl)    procedure 
//(flfloor fl)    procedure 
//(flceiling fl)    procedure 
//(fltruncate fl)    procedure 
//(flround fl)    procedure
//(flexp fl)    procedure 
//(fllog fl)    procedure 
//(fllog fl1 fl2)    procedure 
//(flsin fl)    procedure 
//(flcos fl)    procedure 
//(fltan fl)    procedure 
//(flasin fl)    procedure 
//(flacos fl)    procedure 
//(flatan fl)    procedure 
//(flatan fl1 fl2)    procedure
//(flsqrt fl)    procedure
//(flexpt fl1 fl2)    procedure 
//&no-infinities    condition type 
//&no-nans    condition type 
//(fixnum->flonum fx)    procedure 
//
////        11.4  Exact bitwise arithmetic
//(bitwise-not ei)    procedure
//(bitwise-and ei1 ...)    procedure 
//(bitwise-ior ei1 ...)    procedure 
//(bitwise-xor ei1 ...)    procedure 
//(bitwise-if ei1 ei2 ei3)    procedure
//(bitwise-bit-count ei)    procedure 
//(bitwise-length ei)    procedure
//(bitwise-first-bit-set ei)    procedure 
//(bitwise-bit-set? ei1 ei2)    procedure 
//(bitwise-copy-bit ei1 ei2 ei3)    procedure
//(bitwise-bit-field ei1 ei2 ei3)    procedure
//(bitwise-copy-bit-field ei1 ei2 ei3 ei4)    procedure 
//(bitwise-arithmetic-shift ei1 ei2)    procedure
//(bitwise-arithmetic-shift-left ei1 ei2)    procedure 
//(bitwise-arithmetic-shift-right ei1 ei2)    procedure 
//(bitwise-arithmetic-shift-right ei1 ei2)
//(bitwise-rotate-bit-field ei1 ei2 ei3 ei4)    procedure 
//(bitwise-reverse-bit-field ei1 ei2 ei3)    procedure 


  //
  // Chapter 12 syntax-case
  //

  //
  // Chapter 13 Hashtables
  //

  //13.1  Constructors
  //(define h (make-eq-hashtale)
  //(define h (make-eq-hashtable 1000))
  define_libfunc("make-eq-hashtable", 0, 1, function(ar){
    // Note: ar[1] (hashtable size) is just ignored
    return new Hashtable(Hashtable.eq_hash, Hashtable.eq_equiv);
  });
  //(make-eqv-hashtable)    procedure 
  //(make-eqv-hashtable k)    procedure 
  define_libfunc("make-eqv-hashtable", 0, 1, function(ar){
    return new Hashtable(Hashtable.eqv_hash, Hashtable.eqv_equiv);
  });
  //(make-hashtable hash-function equiv)    procedure 
  //(make-hashtable hash-function equiv k)    procedure
  define_libfunc("make-hashtable", 2, 3, function(ar){
    assert_procedure(ar[0]);
    assert_procedure(ar[1]);
    return new Hashtable(ar[0], ar[1]);
  });

  //13.2  Procedures
  // (hashtable? hash)
  define_libfunc("hashtable?", 1, 1, function(ar){
    return ar[0] instanceof Hashtable;
  });
  //(hashtable-size hash)
  define_libfunc("hashtable-size", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].keys().length;
  });

  //
  // find_hash_pair: the key of the magic :-)
  // takes two callback functions i.e, on_found and on_not_found
  //
  BiwaScheme.find_hash_pair = function(hash, key, callbacks){
    // invoke hash proc 
    return new Call(hash.hash_proc, [key], function(ar){
      var hashed = ar[0];
      var candidate_pairs = hash.candidate_pairs(hashed);

      if (!candidate_pairs){ // shortcut: obviously not found
        return callbacks.on_not_found(hashed);
      }

      // search the exact key from candidates
      return Call.foreach(candidate_pairs, {
        call: function(pair){
          // invoke the equivalence proc
          return new Call(hash.equiv_proc, [key, pair[0]]);
        },
        result: function(equal, pair){
          if(equal) {       // found
            return callbacks.on_found(pair, hashed);
          }
        },
        finish: function(){ // not found
          return callbacks.on_not_found(hashed);
        }
      });
    });
  };

  //(hashtable-ref hash "foo" #f)
  define_libfunc("hashtable-ref", 3, 3, function(ar){
    var hash = ar[0], key = ar[1], ifnone = ar[2];
    assert_hashtable(hash);

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair){
        return pair[1];
      },
      on_not_found: function(hashed){
        return ifnone;
      }
    });
  });

  //(hashtable-set! hash "foo" '(1 2))
  define_libfunc("hashtable-set!", 3, 3, function(ar){
    var hash = ar[0], key = ar[1], value = ar[2];
    assert_hashtable(hash);

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair){
        pair[1] = value;
        return BiwaScheme.undef;
      },
      on_not_found: function(hashed){
        hash.add_pair(hashed, key, value);
        return BiwaScheme.undef;
      }
    });
  });

  //(hashtable-delete! hash "foo")
  define_libfunc("hashtable-delete!", 2, 2, function(ar){
    var hash = ar[0], key = ar[1];
    assert_hashtable(hash);

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair, hashed){
        hash.remove_pair(hashed, pair);
        return BiwaScheme.undef;
      },
      on_not_found: function(hashed){
        return BiwaScheme.undef;
      }
    });
  });

  //(hashtable-contains? hash "foo")
  define_libfunc("hashtable-contains?", 2, 2, function(ar){
    var hash = ar[0], key = ar[1];
    assert_hashtable(hash);

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair){
        return true;
      },
      on_not_found: function(hashed){
        return false;
      }
    });
  });

  //(hashtable-update! hashtable key proc default)    procedure 
  define_libfunc("hashtable-update!", 4, 4, function(ar){
    var hash = ar[0], key = ar[1], proc = ar[2], ifnone = ar[3];
    assert_hashtable(hash);
    assert_procedure(proc);

    return BiwaScheme.find_hash_pair(hash, key, {
      on_found: function(pair, hashed){
        // invoke proc and get new value
        return new Call(proc, [pair[1]], function(ar){
          // replace the value
          pair[1] = ar[0];
          return BiwaScheme.undef;
        });
      },
      on_not_found: function(hashed){
        // invoke proc and get new value
        return new Call(proc, [ifnone], function(ar){
          // create new pair
          hash.add_pair(hashed, key, ar[0]);
          return BiwaScheme.undef;
        });
      }
    });
  });
  //(hashtable-copy hashtable)    procedure 
  //(hashtable-copy hashtable mutable)    procedure 
  define_libfunc("hashtable-copy", 1, 2, function(ar){
    var mutable = (ar[1]===undefined) ? false : !!ar[1];
    assert_hashtable(ar[0]);
    return ar[0].create_copy(mutable);
  });
  //(hashtable-clear! hashtable)    procedure 
  //(hashtable-clear! hashtable k)    procedure 
  define_libfunc("hashtable-clear!", 0, 1, function(ar){
    assert_hashtable(ar[0]);
    ar[0].clear();
    return BiwaScheme.undef;
  });
  //(hashtable-keys hash)  ; => vector
  define_libfunc("hashtable-keys", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].keys();
  });
  //(hashtable-entries hash)  ; => two vectors (keys, values)
  define_libfunc("hashtable-entries", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return new Values([ar[0].keys(), ar[0].values()]);
  });

  //13.3  Inspection
  
  //(hashtable-equivalence-function hashtable)    procedure 
  define_libfunc("hashtable-equivalence-function", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].equiv_proc;
  });
  //(hashtable-hash-function hashtable)    procedure 
  define_libfunc("hashtable-hash-function", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].hash_proc;
  });
  //(hashtable-mutable? hashtable)    procedure 
  define_libfunc("hashtable-mutable?", 1, 1, function(ar){
    assert_hashtable(ar[0]);
    return ar[0].mutable;
  });

  //13.4  Hash functions

  //(equal-hash obj)    procedure 
  define_libfunc("equal-hash", 0, 0, function(ar){
    return Hashtable.equal_hash;
  });
  //(string-hash string)    procedure
  define_libfunc("string-hash", 0, 0, function(ar){
    return Hashtable.string_hash;
  });
  //(string-ci-hash string)    procedure
  define_libfunc("string-ci-hash", 0, 0, function(ar){
    return Hashtable.string_ci_hash;
  });
  //(symbol-hash symbol)    procedure
  define_libfunc("symbol-hash", 0, 0, function(ar){
    return Hashtable.symbol_hash;
  });

  //
  // Chapter 14 Enumerators
  //
//(make-enumeration symbol-list)    procedure 
//(enum-set-universe enum-set)    procedure 
//(enum-set-indexer enum-set)    procedure
//(enum-set-constructor enum-set)    procedure 
//(enum-set->list enum-set)    procedure
//(enum-set-member? symbol enum-set)    procedure 
//(enum-set-subset? enum-set1 enum-set2)    procedure 
//(enum-set=? enum-set1 enum-set2)    procedure 
//(enum-set-union enum-set1 enum-set2)    procedure 
//(enum-set-intersection enum-set1 enum-set2)    procedure 
//(enum-set-difference enum-set1 enum-set2)    procedure 
//(enum-set-complement enum-set)    procedure 
//(enum-set-projection enum-set1 enum-set2)    procedure 
//(define-enumeration <type-name>    syntax 
//(<symbol> ...)
//<constructor-syntax>)

  //
  // Chapter 15 Composite library
  //
  //(rnrs 6) = all - eval - mutable pairs - mutable strings - r5rs compatibility

  //
  // Chapter 16 eval
  //
  //(eval expression environment)    procedure 
  define_libfunc("eval", 1, 1, function(ar, intp){
    //TODO: environment
    //TODO: this implementation has a bug that
    //  expressions which contains #<undef>, etc. cannot be evaluated.
    var expr = ar[0];
    var intp = new BiwaScheme.Interpreter(intp.on_error);
    
    return intp.evaluate(expr.to_write());
  });
//(environment import-spec ...)    procedure

  //
  // Chapter 17 Mutable pairs
  //
//(set-car! pair obj)    procedure
//(set-cdr! pair obj)    procedure

  //
  // Chapter 18 Mutable strings
  //
  //(string-set! string k char)    procedure
 // (string-fill! string char)    procedure 
 
  //
  // Chapter 19 R5RS compatibility
  //
//(exact->inexact z)    procedure 
//(inexact->exact z)    procedure 
//
//(quotient n1 n2)    procedure 
//(remainder n1 n2)    procedure 
//(modulo n1 n2)    procedure
//
//(delay <expression>)    syntax  
//(force promise)    procedure 
//(make-promise (lambda () <expression>))
//
//(null-environment n)    procedure 
//(scheme-report-environment n)    procedure 

  /* --------------------------------------- namespace webscheme */ 
}
if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {

  //
  // interface to javascript
  //
  define_libfunc_raw("js-eval", 1, 1, function(ar){
    return eval(ar[0]);
  });
  define_libfunc_raw("js-ref", 2, 2, function(ar){
    assert_string(ar[1]);
    return ar[0][ar[1]]; //todo: first-class js function
  });
  define_libfunc("js-set!", 3, 3, function(ar){
    assert_string(ar[1]);
    ar[0][ar[1]] = ar[2];
    return BiwaScheme.undef;
  });

  // (js-call (js-eval "Math.pow") 2 4)
  define_libfunc_raw("js-call", 1, null, function(ar){
    var js_func = ar.shift();
    assert_function(js_func);

    var receiver = null;
    return js_func.apply(receiver, ar);
  });
  // (js-invoke (js-new "Date") "getTime")
  define_libfunc_raw("js-invoke", 2, null, function(ar){
    var js_obj = ar.shift();
    var func_name = ar.shift();
    assert_string(func_name);
    if(js_obj[func_name])
      return js_obj[func_name].apply(js_obj, ar);
    else
      throw new Error("js-invoke: function "+func_name+" is not defined");
  });

  // (js-new "Date" 2005 1 1)
  // (js-new "Draggable elem 'onEnd (lambda (drg) ...))
  //   If symbol is given, following arguments are converted to 
  //   an js object. If any of them is a scheme closure,
  //   it is converted to js function which invokes that closure.
  //
  define_libfunc("js-new", 1, null, function(ar, intp){
    // make js object from key-value pair
    var array_to_obj = function(ary){
      if((ary.length % 2) != 0)
        throw new Error("js-new: odd number of key-value pair");

      var obj = {};
      for(var i=0; i<ary.length; i+=2){
        var key = ary[i], value = ary[i+1];
        assert_symbol(key);
        if(value.closure_p === true)
          value = BiwaScheme.js_closure(value, intp);

        obj[key.name] = value;
      }
      return obj;
    };

    var ctor = ar.shift();
    assert_string(ctor);
    if(ar.length == 0){
      return eval("new " + ctor + "()");
    }
    else{
      // pack args to js object, if symbol appears
      var args = [];
      for(var i=0; i<ar.length; i++){
        if(ar[i] instanceof Symbol){
          args.push(array_to_obj(ar.slice(i)));
          break;
        }
        else{
          args.push(ar[i]);
        }
      }
      // construct js code to create new object
      var args_str = _.map(ar, function(value, i){
        return "args['" + i + "']";
      }).join(",");
      return eval("new " + ctor + "(" + args_str + ")");
    }
  });

  // (js-obj "foo" 1 "bar" 2)
  // -> {"foo": 1, "bar": 2}
  define_libfunc("js-obj", 0, null, function(ar){
    if(ar.length % 2 != 0){
      throw new Error("js-obj: number of arguments must be even");
    }

    var obj = {};
    for(i=0; i<ar.length/2; i++){
      assert_string(ar[i*2]);
      obj[ar[i*2]] = ar[i*2+1];
    }
    return obj;
  });

  // (js-closure (lambda (event) ..))
  BiwaScheme.js_closure = function(proc, intp){
    var on_error = intp.on_error;
    return function(/*args*/){
      var intp = new Interpreter(on_error);
      return intp.invoke_closure(proc, _.toArray(arguments));
    };
  };
  define_libfunc("js-closure", 1, 1, function(ar, intp){
    assert_closure(ar[0]);
    return BiwaScheme.js_closure(ar[0], intp);
  });

  define_libfunc("js-null?", 1, 1, function(ar){
    return ar[0] === null;
  });

  define_libfunc("js-undefined?", 1, 1, function(ar){
    return ar[0] === undefined;
  });

  define_libfunc_raw("js-array-to-list", 1, 1, function(ar){
    return BiwaScheme.array_to_list(ar[0]);
  });

  define_libfunc_raw("list-to-js-array", 1, 1, function(ar){
    return ar[0].to_array();
  });

  //
  // timer, sleep
  //
  define_libfunc("timer", 2, 2, function(ar, intp){
    var proc = ar[0], sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    setTimeout(function(){ (new Interpreter(intp.on_error)).invoke_closure(proc); }, sec * 1000);
    return BiwaScheme.undef;
  });
  define_libfunc("set-timer!", 2, 2, function(ar, intp){
    var proc = ar[0], sec = ar[1];
    assert_closure(proc);
    assert_real(sec);
    return setInterval(function(){ (new Interpreter(intp.on_error)).invoke_closure(proc); }, sec * 1000);
  });
  define_libfunc("clear-timer!", 1, 1, function(ar){
    var timer_id = ar[0];
    clearInterval(timer_id);
    return BiwaScheme.undef;
  });
  define_libfunc("sleep", 1, 1, function(ar){
    var sec = ar[0];
    assert_real(sec);
    return new BiwaScheme.Pause(function(pause){
      setTimeout(function(){ pause.resume(nil); }, sec * 1000);
    });
  });

}

if( typeof(BiwaScheme)!='object' ) BiwaScheme={}; with(BiwaScheme) {
  define_libfunc("html-escape", 1, 1, function(ar){
    assert_string(ar[0]);
    return _.escapeHTML(ar[0]);
  });
  BiwaScheme.inspect_objs = function(objs){
    return _.map(objs, BiwaScheme.inspect).join(", ");
  };
  define_libfunc("inspect", 1, null, function(ar){
    return BiwaScheme.inspect_objs(ar);
  });
  define_libfunc("inspect!", 1, null, function(ar){
    puts(BiwaScheme.inspect_objs(ar));
    return BiwaScheme.undef;
  });

  //
  // json
  //
  // json->sexp
  // Array -> list
  // Object -> alist
  // (number, boolean, string, 
  //
  BiwaScheme.json2sexp = function(json){
    switch(true){
    case _.isNumber(json) ||
         _.isString(json) ||
         json === true || json === false:
      return json;
    case _.isArray(json):
      return array_to_list(_.map(json, json2sexp));
    case typeof(json) == "object":
      var ls = nil;
      for(key in json){
        ls = new Pair(new Pair(key, json2sexp(json[key])),
               ls);
      }
      return ls;
    default:
      throw new Error("json->sexp: detected invalid value for json: "+BiwaScheme.inspect(json));
    }
    throw new Bug("must not happen");
  }
  define_libfunc("json->sexp", 1, 1, function(ar){
    return json2sexp(ar[0]);
  })

  //
  //from Gauche
  //

  define_libfunc("identity", 1, 1, function(ar){
    return ar[0];
  });

  // string
  
  define_libfunc("string-concat", 1, 1, function(ar){
    assert_list(ar[0]);
    return ar[0].to_array().join("");
  })

  define_libfunc("string-split", 2, 2, function(ar){
    assert_string(ar[0]);
    assert_string(ar[1]);
    return array_to_list(ar[0].split(ar[1]));
  })

  define_libfunc("string-join", 1, 2, function(ar){
    assert_list(ar[0]);
    var delim = ""
    if(ar[1]){
      assert_string(ar[1]);
      delim = ar[1];
    }
    return ar[0].to_array().join(delim);
  })
  
  // lists

  define_libfunc("intersperse", 2, 2, function(ar){
    var item = ar[0], ls = ar[1];
    assert_list(ls);

    var ret = [];
    _.each(ls.to_array().reverse(),function(x){
      ret.push(x);
      ret.push(item);
    });
    ret.pop();
    return array_to_list(ret);
  });

  define_libfunc("map-with-index", 2, null, function(ar){
    var proc = ar.shift(), lists = ar;
    _.each(lists, assert_list);

    var results = [], i = 0;
    return Call.multi_foreach(lists, {
      call: function(xs){ 
        var args = _.map(xs, function(x){ return x.car });
        args.unshift(i);
        i++;
        return new Call(proc, args);
      },
      result: function(res){ results.push(res); },
      finish: function(){ return array_to_list(results); }
    });
  });

  // sorting
  
  // These functions takes a Scheme proc and sort the given
  // list or vector using the proc as the compare function.
  //
  // Limitations:
  //  - you should not use Ajax or sleep inside the proc
  //  - you cannot access to the free variables outside the proc
  //
  // (list-sort/comp proc list)
  // (vector-sort/comp proc vector)
  // (vector-sort/comp! proc vector)

  // utility function. takes a JS Array and a Scheme procedure,
  // returns sorted array
  var sort_with_comp = function(ary, proc){
    return ary.sort(function(a, b){
        var intp2 = new BiwaScheme.Interpreter();
        return intp2.invoke_closure(proc, [a, b]);
      });
  };

  define_libfunc("list-sort/comp", 1, 2, function(ar){
    assert_procedure(ar[0]);
    assert_list(ar[1]);

    return array_to_list(sort_with_comp(ar[1].to_array(), ar[0]));
  });
  define_libfunc("vector-sort/comp", 1, 2, function(ar){
    assert_procedure(ar[0]);
    assert_vector(ar[1]);

    return sort_with_comp(_.clone(ar[1]), ar[0]);
  });
  define_libfunc("vector-sort/comp!", 1, 2, function(ar){
    assert_procedure(ar[0]);
    assert_vector(ar[1]);

    sort_with_comp(ar[1], ar[0]);
    return BiwaScheme.undef;
  });
  
  // macros

  //(define-macro (foo x) body ...)
  //(define-macro foo lambda)

  var rearrange_args = function (expected, given) {
    var args = [];
    var dotpos = (new Compiler).find_dot_pos(expected);
    if (dotpos == -1)
      args = given;
    else {
      for (var i = 0; i < dotpos; i++) {
        args[i] = given[i];
      }
      args[i] = array_to_list(given.slice(i));
    }
    return args;
  }
  define_syntax("define-macro", function(x){
    var head = x.cdr.car;
    var expected_args;
    if(head instanceof Pair){
      var name = head.car;
      expected_args = head.cdr;
      var body = x.cdr.cdr;
      var lambda = new Pair(Sym("lambda"),
                     new Pair(expected_args,
                       body))
    }
    else{
      var name = head;
      var lambda = x.cdr.cdr.car;
      expected_args = lambda.cdr.car;
    }

    //[close, n_frees, do_body, next]
    var opc = Compiler.compile(lambda);
    if(opc[1] != 0)
      throw new Bug("you cannot use free variables in macro expander (or define-macro must be on toplevel)")
    var cls = [opc[2]];

    TopEnv[name.name] = new Syntax(name.name, function(sexp){
      var given_args = sexp.to_array();

      given_args.shift();
      
      var intp = new Interpreter();
      var args = rearrange_args(expected_args, given_args);
      var result = intp.invoke_closure(cls, args);
      return result;
    });

    return BiwaScheme.undef;
  })

  var macroexpand_1 = function(x){
    if(x instanceof Pair){
      if(x.car instanceof Symbol && TopEnv[x.car.name] instanceof Syntax){
        var transformer = TopEnv[x.car.name];
        x = transformer.transform(x);
      }
      else
        throw new Error("macroexpand-1: `" + to_write_ss(x) + "' is not a macro");
    }
    return x;
  }
  define_syntax("%macroexpand", function(x){
    var expanded = (new Interpreter).expand(x.cdr.car);
    return List(Sym("quote"), expanded);
  });
  define_syntax("%macroexpand-1", function(x){
    var expanded = macroexpand_1(x.cdr.car);
    return List(Sym("quote"), expanded);
  });

  define_libfunc("macroexpand", 1, 1, function(ar){
    return (new Interpreter).expand(ar[0]);
  });
  define_libfunc("macroexpand-1", 1, 1, function(ar){
    return macroexpand_1(ar[0]);
  });

  define_libfunc("gensym", 0, 0, function(ar){
    return BiwaScheme.gensym();
  });
  
  // i/o

  define_libfunc("print", 1, null, function(ar){
    _.map(ar, function(item){
      puts(to_display(item), true);
    })
    puts(""); //newline
    return BiwaScheme.undef;
  })
  define_libfunc("write-to-string", 1, 1, function(ar){
    return to_write(ar[0]);
  });
  define_libfunc("read-from-string", 1, 1, function(ar){
    assert_string(ar[0]);
    return Interpreter.read(ar[0]);
  });
  define_libfunc("port-closed?", 1, 1, function(ar){
    assert_port(ar[0]);
    return !(ar[0].is_open);
  });
  
  // syntax
  
  define_syntax("let1", function(x){
    //(let1 vari expr body ...) 
    //=> ((lambda (var) body ...) expr)
    var vari = x.cdr.car; 
    var expr = x.cdr.cdr.car;
    var body = x.cdr.cdr.cdr;

    return new Pair(new Pair(Sym("lambda"),
                      new Pair(new Pair(vari, nil),
                        body)),
             new Pair(expr, nil));
  })

  //
  // Regular Expression
  //
  var assert_regexp = function(obj, fname){
    if(!(obj instanceof RegExp))
      throw new Error(fname + ": regexp required, but got " + to_write(obj));
  }

  //Function: string->regexp string &keyword case-fold 
  define_libfunc("string->regexp", 1, 1, function(ar){
    assert_string(ar[0], "string->regexp");
    return new RegExp(ar[0]); //todo: case-fold
  })
  //Function: regexp? obj 
  define_libfunc("regexp?", 1, 1, function(ar){
    return (ar[0] instanceof RegExp);
  })
  //Function: regexp->string regexp 
  define_libfunc("regexp->string", 1, 1, function(ar){
    assert_regexp(ar[0], "regexp->string");
    return ar[0].toString().slice(1, -1); //cut '/' 
  })

  define_libfunc("regexp-exec", 2, 2, function(ar){
    var rexp = ar[0];
    if(_.isString(ar[0])){
      rexp = new RegExp(ar[0]);
    }
    assert_regexp(rexp, "regexp-exec");
    assert_string(ar[1], "regexp-exec");
    var ret = rexp.exec(ar[1])
    return (ret === null) ? false : array_to_list(ret);
  })

//  //Function: rxmatch regexp string 
//  define_libfunc("rxmatch", 1, 1, function(ar){
//    assert_regexp(ar[0], "rxmatch");
//    assert_string(ar[1], "rxmatch");
//    return ar[0].match(ar[1]);
//  });
  //Function: rxmatch-start match &optional (i 0) 
  //Function: rxmatch-end match &optional (i 0) 
  //Function: rxmatch-substring match &optional (i 0) 
  //Function: rxmatch-num-matches match   
  //Function: rxmatch-after match &optional (i 0) 
  //Function: rxmatch-before match &optional (i 0) 
  //Generic application: regmatch &optional index 
  //Generic application: regmatch 'before &optional index 
  //Generic application: regmatch 'after &optional index 
  //Function: regexp-replace regexp string substitution 
  //Function: regexp-replace-all regexp string substitution 
  //Function: regexp-replace* string rx1 sub1 rx2 sub2 ... 
  //Function: regexp-replace-all* string rx1 sub1 rx2 sub2 ... 
  //Function: regexp-quote string 
  //Macro: rxmatch-let match-expr (var ...) form ... 
  //Macro: rxmatch-if match-expr (var ...) then-form else-form 
  //Macro: rxmatch-cond clause ... 
  //Macro: rxmatch-case string-expr clause ... 

}


//
// srfi.js - SRFI libraries
//
// should be src/library/srfi/1.js, etc (in the future).
//

with(BiwaScheme) {
  
  //
  // srfi-1 (list)
  //
  define_libfunc("iota", 1, 3, function(ar){
    var count = ar[0];
    var start = ar[1] || 0;
    var step = (ar[2]===undefined) ? 1 : ar[2];
    assert_integer(count);
    assert_number(start);
    assert_number(step);

    var a = [], n = start;
    for(var i=0; i<count; i++){
      a.push(n);
      n += step;
    }
    return array_to_list(a);
  });

  //
  // srfi-6 & Gauche (string port)
  // 
  define_libfunc("open-input-string", 1, 1, function(ar){
    assert_string(ar[0]);
    return new Port.StringInput(ar[0]);
  })
  define_libfunc("open-output-string", 0, 0, function(ar){
    return new Port.StringOutput();
  })
  define_libfunc("get-output-string", 1, 1, function(ar){
    assert_port(ar[0]);
    if(!(ar[0] instanceof Port.StringOutput))
      throw new Error("get-output-string: port must be made by 'open-output-string'");
    return ar[0].output_string();
  })

  // srfi-19 (time)
  //
//  // constants
//time-duration
//time-monotonic
//time-process
//time-tai
//time-thread
//time-utc
  // Current time and clock resolution
  define_libfunc("current-date", 0, 1, function(ar){
    //todo: tz-offset (ar[1])
    return new Date();
  })
//
//current-julian-day -> jdn
//current-modified-julian-day -> mjdn
//current-time [time-type] -> time
//time-resolution [time-type] -> integer
//  // Time object and accessors
//make-time type nanosecond second -> time
//time? object -> boolean
//time-type time -> time-type
//time-nanosecond time -> integer
//time-second time -> integer
//set-time-type! time time-type
//set-time-nanosecond! time integer
//set-time-second! time integer
//copy-time time1 -> time2 
//  // Time comparison procedures
//time<=? time1 time2 -> boolean
//time<? time1 time2 -> boolean
//time=? time1 time2 -> boolean
//time>=? time1 time2 -> boolean
//time>? time1 time2 -> boolean
//  // Time arithmetic procedures
//time-difference time1 time2 -> time-duration
//time-difference! time1 time2 -> time-duration
//add-duration time1 time-duration -> time
//add-duration! time1 time-duration -> time
//subtract-duration time1 time-duration -> time
//subtract-duration! time1 time-duration -> time
  // Date object and accessors
  // make-date
  define_libfunc("date?", 1, 1, function(ar){
    return (ar[0] instanceof Date);
  })
  define_libfunc("date-nanosecond", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMilliseconds() * 1000000;
  })
  define_libfunc("date-millisecond", 1, 1, function(ar){ // not srfi-19
    assert_date(ar[0]);
    return ar[0].getMilliseconds();
  })
  define_libfunc("date-second", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getSeconds();
  })
  define_libfunc("date-minute", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMinutes();
  })
  define_libfunc("date-hour", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getHours();
  })
  define_libfunc("date-day", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getDate();
  })
  define_libfunc("date-month", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getMonth() + 1; //Jan = 0 in javascript..
  })
  define_libfunc("date-year", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getFullYear();
  })
  //date-zone-offset
  //date-year-day
  define_libfunc("date-week-day", 1, 1, function(ar){
    assert_date(ar[0]);
    return ar[0].getDay();
  })
  //date-week-number

  // Time/Date/Julian Day/Modified Julian Day Converters
  // (snipped)
  
  // Date to String/String to Date Converters
  // TODO: support locale
  //   * date_names
  //   * ~f 5.2 sec
  //   * ~p AM/PM
  //   * ~X 2007/01/01
  BiwaScheme.date_names = {
    weekday: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
    full_weekday: ["Sunday", "Monday", "Tuesday", 
      "Wednesday", "Thursday", "Friday", "Saturday"],
    month: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", 
      "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    full_month: ["January", "February", "March", "April",
      "May", "June", "July", "August", "September", 
      "Octorber", "November", "December"]
  }

  BiwaScheme.date2string = function(date, format){
    var zeropad  = function(n){ return n<10 ? "0"+n : ""+n }; 
    var spacepad = function(n){ return n<10 ? " "+n : ""+n }; 
    
    var getter = {
      a: function(x){ return date_names.weekday[x.getDay()] },
      A: function(x){ return date_names.full_weekday[x.getDay()] },
      b: function(x){ return date_names.month[x.getMonth()] },
      B: function(x){ return date_names.full_month[x.getMonth()] },
      c: function(x){ return x.toString() },
      d: function(x){ return zeropad(x.getDate()) },
      D: function(x){ return getter.d(x) + getter.m(x) + getter.y(x); },
      e: function(x){ return spacepad(x.getDate()) },
      f: function(x){ return x.getSeconds() + x.getMilliseconds()/1000; },
      h: function(x){ return date_names.month[x.getMonth()] },
      H: function(x){ return zeropad(x.getHours()) },
      I: function(x){ var h = x.getHours(); return zeropad(h<13 ? h : h-12) },
      j: function(x){ throw new Bug("not implemented: day of year") },
      k: function(x){ return spacepad(x.getHours()) },
      l: function(x){ var h = x.getHours(); return spacepad(h<13 ? h : h-12) },
      m: function(x){ return zeropad(x.getMonth()) },
      M: function(x){ return zeropad(x.getMinutes()) },
      n: function(x){ return "\n" },
      N: function(x){ throw new Bug("not implemented: nanoseconds") },
      p: function(x){ return x.getHours()<13 ? "AM" : "PM" },
      r: function(x){ return getter.I(x) + ":" + getter.M(x) + ":" + getter.S(x) + " " + getter.p(x) },
      s: function(x){ return Math.floor(x.getTime() / 1000) },
      S: function(x){ return zeropad(x.getSeconds()) },
      t: function(x){ return "\t" },
      T: function(x){ return getter.H(x) + ":" + getter.M(x) + ":" + getter.S(x) },
      U: function(x){ throw new Bug("not implemented: weeknum(0~, Sun)") },
      V: function(x){ throw new Bug("not implemented: weeknum(1~, Sun?)") },
      w: function(x){ return x.getDay() },
      W: function(x){ throw new Bug("not implemented: weeknum(0~, Mon)") },
      x: function(x){ throw new Bug("not implemented: weeknum(1~, Mon)") },
      X: function(x){ return getter.Y(x) + "/" + getter.m(x) + "/" + getter.d(x) },
      y: function(x){ return x.getFullYear() % 100 },
      Y: function(x){ return x.getFullYear() },
      z: function(x){ throw new Bug("not implemented: time-zone") },
      Z: function(x){ throw new Bug("not implemented: symbol time zone") },
      1: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day format") },
      2: function(x){ throw new Bug("not implemented: ISO-8601 hour-minute-second-timezone format") },
      3: function(x){ throw new Bug("not implemented: ISO-8601 hour-minute-second format") },
      4: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second-timezone format") },
      5: function(x){ throw new Bug("not implemented: ISO-8601 year-month-day-hour-minute-second format") }
    }

    return format.replace(/~([\w1-5~])/g, function(str, x){
      var func = getter[x];
      if(func)
        return func(date);
      else if(x == "~")
        return "~";
      else
        return x;
    })
  }
  
  // date->string
  define_libfunc("date->string", 1, 2, function(ar){
    assert_date(ar[0]);

    if(ar[1]){
      assert_string(ar[1]);
      return date2string(ar[0], ar[1]);
    }
    else
      return ar[0].toString();
  })
  // string->date

  define_libfunc("parse-date", 1, 1, function(ar){ // not srfi-19
    assert_string(ar[0]);
    return new Date(Date.parse(ar[0]));
  })

  //
  // srfi-27
  //
  define_libfunc("random-integer", 1, 1, function(ar){
    var n = ar[0];
    assert_integer(n);
    if (n < 0)
      throw new Error("random-integer: the argument must be >= 0");
    else
      return Math.floor(Math.random() * ar[0]);
  });
  define_libfunc("random-real", 0, 0, function(ar){
    return Math.random();
  });

  //
  // srfi-38 (write/ss)
  //
  var user_write_ss = function(ar){
    puts(write_ss(ar[0]), true);
    return BiwaScheme.undef;
  }
  define_libfunc("write/ss", 1, 2, user_write_ss);
  define_libfunc("write-with-shared-structure", 1, 2, user_write_ss);
  define_libfunc("write*", 1, 2, user_write_ss); //from Gauche(STklos)

  //
  // srfi-43 (vector library)
  //
  define_libfunc("vector-append", 2, null, function(ar){
    var vec = [];
    return vec.concat.apply(vec, ar);
  })
}
>>>>>>> 9ebe216613c4049578b7b0ae0a92382453c715e6
