//
// Dumper - graphical state dumper
//
with(BiwaScheme) {

BiwaScheme.Dumper = Class.create({
  initialize: function(dumparea){
    this.dumparea = dumparea || $("dumparea") || null;;
    this.reset();
  },

  reset: function(){
    if(this.dumparea){
      // Note: this is for repl.html (needs refactoring..)
      $(this.dumparea).update("");
    }
    this.n_folds = 0;
    this.closures = [];
    this.n_dumps = 0;
    this.cur = -1;
    this.is_folded = true;
  },

  is_opc: function(obj){
    return (obj instanceof Array && typeof(obj[0]) == 'string');
  },

  dump_pad: "&nbsp;&nbsp;&nbsp;",
  dump_opc: function(obj, level){
    var s="";
    var pad1="", pad2="";
    var level = level || 0;
    level.times(function(){ pad1 += this.dump_pad; }.bind(this));
    (level+1).times(function(){ pad2 += this.dump_pad; }.bind(this));

    s += pad1 + '[<span class="dump_opecode">' + obj[0] + '</span>';
    var i = 1;
    while(!(obj[i] instanceof Array) && i<obj.length){
      if(obj[0] == "constant")
        s += "&nbsp;<span class='dump_constant'>" + 
             this.dump_obj(obj[i]) + "</span>";
      else
        s += "&nbsp;" + this.dump_obj(obj[i]);
      i++;
    }
    if(i < obj.length) s += '<br>\n';
    for(; i<obj.length; i++){
      if(this.is_opc(obj[i])){
        s += this.dump_opc(obj[i], (i == obj.length-1 ? level : level+1));
      }
      else{
        s += (i == obj.length-1) ? pad1 : pad2;
        s += this.dump_obj(obj[i]); //String(obj[i]).escapeHTML();
      }
      if(i != obj.length-1) s += "<br>\n";
    }
    s += "]";
    return (level==0 ? this.add_fold(s) : s);
  },

  fold_limit: 20,
  add_fold: function(s){
    var lines = s.split(/<br>/gmi);

    if(lines.length > this.fold_limit){
      var fold_btn   = " <span style='text-decoration:underline; color:blue; cursor:pointer;'" +
                           "onclick='BiwaScheme.Dumper.toggle_fold("+this.n_folds+")'>more</span>"
      var fold_start = "<div style='display:none' id='fold"+this.n_folds+"'>"; 
      var fold_end   = "</div>"
      this.n_folds++;
      return [
        lines.slice(0, this.fold_limit).join("<br>"), fold_btn,
        fold_start, lines.slice(this.fold_limit+1).join("<br>"), fold_end
      ].join("");
    }
    else{
      return s;
    }
  },

  stack_max_len: 80,
  dump_stack: function(stk, size){
    if(stk === null || stk === undefined) return Object.inspect(stk)
    var s = "<table>";

    // show the 'physical' stack top
    if (stk.length == 0){
      s += "<tr><td class='dump_dead'>(stack is empty)</td></tr>";
    }
    else if (size < stk.length){
      var l = stk.length - 1;
      s += "<tr><td class='dump_dead'>[" + l + "]</td>" +
           "<td class='dump_dead'>" + 
           this.dump_obj(stk[l]).truncate(this.stack_max_len) +
           "</td></tr>";
    }

    // show the element in the stack
    for(var i=size-1; i >= 0; i--){
      s += "<tr><td class='dump_stknum'>[" + i + "]</td>" +
           "<td>" + this.dump_obj(stk[i]).truncate(this.stack_max_len) +
           "</td></tr>";
    }
    return s + "</table>";
  },

  dump_object: function(obj){
    var a = [];
    for(var k in obj){
      //if(this.prototype[k]) continue;
      a.push( k.toString() );//+" => "+this[k].toString() );
    }
    return "#<Object{"+a.join(",")+"}>";
  },

  dump_closure: function(cls){
    if(cls.length == 0) return "[]";

    var cls_num = null;
    for(var i=0; i<this.closures.length; i++){
      if(this.closures[i] == cls) cls_num = i;
    }
    if(cls_num == null){
      cls_num = this.closures.length;
      this.closures.push(cls);
    }

    var c = cls.clone ? cls.clone() : [c];
    var body = c.shift();
    return [
      "c", cls_num, " <span class='dump_closure'>free vars :</span> ",
      this.dump_obj(c), " <span class='dump_closure'>body :</span> ",
      this.dump_obj(body).truncate(100)
    ].join("");
  },

  dump_obj: function(obj){
    if(obj && typeof(obj.to_html) == 'function')
      return obj.to_html();
    else{
      var s = write_ss(obj, true); //true=Array mode
      if(s == "[object Object]") s = this.dump_object(obj);
      return s.escapeHTML();
    }
  },

  dump: function(obj){
    var dumpitem = document.createElement("div"); 
    var s = "";
    if(obj instanceof Hash){
      s += "<table>"

      // header
      s += "<tr><td colspan='4'>" + 
           "<a href='#' id='dump_" + this.n_dumps + "_header'>" +
           "#"+this.n_dumps+"</a></td></tr>";

      // registers
      obj.each(function(pair){
        if(pair.key!="x" && pair.key != "stack"){
          var value = (pair.key=="c" ? this.dump_closure(pair.value) 
                                     : this.dump_obj(pair.value))
          s += "<tr><td>" + pair.key + ": </td>" + 
               "<td colspan='3'>" + value + "</td></tr>";
        }
      }.bind(this));
      s += "<tr><td>x:</td><td>" + 
           (this.is_opc(obj.get("x")) ? this.dump_opc(obj.get("x")) 
                                      : this.dump_obj(obj.get("x"))) +
           "</td>";

      // stack
      s += "<td style='border-left: 1px solid black'>stack:</td><td>" +
           this.dump_stack(obj.get("stack"), obj.get("s")) +
           "</td></tr>";
      s += "</table>";
    }
    else{
      s = Object.inspect(obj).escapeHTML() + "<br>\n";
    }
    dumpitem.id = "dump" + this.n_dumps;
    dumpitem.innerHTML = s;
    this.dumparea.appendChild(dumpitem);
    (function(n){
      $("dump_"+this.n_dumps+"_header").observe("click", function(){
        this.dump_move_to(n);
        this.dump_fold();
      }.bind(this));
    }.bind(this))(this.n_dumps);
    Element.hide(dumpitem);
    this.n_dumps++;
  },

  //
  // UI
  //
  dump_move_to: function(n){
    if (0 <= n && n <= this.n_dumps){
      Element.hide($("dump"+this.cur));
      this.cur = n;
      Element.show($("dump"+this.cur));
    }
  },

  dump_move: function(dir){
    if(0 <= this.cur && this.cur < this.n_dumps)
      Element.hide($("dump"+this.cur));

    if(0 <= this.cur+dir && this.cur+dir < this.n_dumps)
      this.cur += dir;

    Element.show($("dump"+this.cur));
  },

  dump_fold: function(){
    for(var i=0; i<this.n_dumps; i++)
      if(i!=this.cur) Element.hide($("dump"+i));

    this.is_folded = true;
  },

  dump_unfold: function(){
    for(var i=0; i<this.n_dumps; i++)
      Element.show($("dump"+i));

    this.is_folded = false;
  },

  dump_toggle_fold: function(){
    if(this.is_folded)
      this.dump_unfold();
    else
      this.dump_fold();
  }
})
} // with(BiwaScheme);

BiwaScheme.Dumper.toggle_fold = function(n){
  Element.toggle("fold"+n);
};
