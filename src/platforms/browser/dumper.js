import * as _ from "../../deps/underscore-1.10.2-esm.js"
import Class from "../../system/class.js"
import { inspect, truncate } from "../../system/_writer.js"
import { write_ss } from "../../system/write_ss.js"
//
// Dumper - graphical state dumper
//

const Dumper = Class.create({
  initialize: function(dumparea){
    this.dumparea = dumparea || $("#dumparea")[0] || null;
    this.reset();
  },

  reset: function(){
    if(this.dumparea){
      // Note: this is for repl.html (needs refactoring..)
      $(this.dumparea).empty();
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
  dump_opc: function(obj, level, nested){
    var s="";
    var pad1="", pad2="";
    var level = level || 0;
    var nested = nested || false;
    _.times(level, _.bind(function(){ pad1 += this.dump_pad; }, this));
    _.times((level+1), _.bind(function(){ pad2 += this.dump_pad; }, this));

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
        s += this.dump_opc(obj[i], (i == obj.length-1 ? level : level+1), true);
      }
      else{
        s += (i == obj.length-1) ? pad1 : pad2;
        s += this.dump_obj(obj[i]);
      }
      if(i != obj.length-1) s += "<br>\n";
    }
    s += "]";
    return (nested ? s : this.add_fold(s));
  },

  fold_limit: 20,
  add_fold: function(s){
    var lines = s.split(/<br>/gmi);

    if(lines.length > this.fold_limit){
      var fold_btn   = " <span style='text-decoration:underline; color:blue; cursor:pointer;'" +
                           "onclick='BiwaScheme.Dumper.toggle_fold("+this.n_folds+")'>more</span>";
      var fold_start = "<div style='display:none' class='fold"+this.n_folds+"'>";
      var fold_end   = "</div>";
      this.n_folds++;
      return [
        lines.slice(0, this.fold_limit).join("<br>"), fold_btn,
        fold_start, lines.slice(this.fold_limit).join("<br>"), fold_end
      ].join("");
    }
    else{
      return s;
    }
  },

  stack_max_len: 80,
  dump_stack: function(stk, size){
    if(stk === null || stk === undefined) return inspect(stk);
    var s = "<table>";

    // show the 'physical' stack top
    if (stk.length == 0){
      s += "<tr><td class='dump_dead'>(stack is empty)</td></tr>";
    }
    else if (size < stk.length){
      var l = stk.length - 1;
      s += "<tr><td class='dump_dead'>[" + l + "]</td>" +
           "<td class='dump_dead'>" + 
           truncate(this.dump_obj(stk[l]), this.stack_max_len) +
           "</td></tr>";
    }

    // show the element in the stack
    for(var i=size-1; i >= 0; i--){
      s += "<tr><td class='dump_stknum'>[" + i + "]</td>" +
           "<td>" + truncate(this.dump_obj(stk[i]), this.stack_max_len) +
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
    if(!cls) return "**BROKEN**";
    if(cls.length == 0) return "[]";

    var cls_num = null;
    for(var i=0; i<this.closures.length; i++){
      if(this.closures[i] == cls) cls_num = i;
    }
    if(cls_num == null){
      cls_num = this.closures.length;
      this.closures.push(cls);
    }

    var c = _.clone(cls);
    var body = c.shift && c.shift();
    return [
      "c", cls_num, " <span class='dump_closure'>free vars :</span> ",
      this.dump_obj(c), " <span class='dump_closure'>body :</span> ",
      truncate(this.dump_obj(body), 100)
    ].join("");
  },

  dump_obj: function(obj){
    if(obj && typeof(obj.to_html) == 'function')
      return obj.to_html();
    else{
      var s = write_ss(obj, true); //true=Array mode
      if(s == "[object Object]") s = this.dump_object(obj);
      return _.escape(s);
    }
  },

  dump: function(obj){
    var s = "";
    if(obj instanceof Object){
      s += "<table>";

      // header
      s += "<tr><td colspan='4'>" + 
           "<a href='#' class='header'>" +
           "#"+this.n_dumps+"</a></td></tr>";

      // registers
      _.each(_.keys(obj), _.bind(function(key){
        var value = obj[key];
        if(key!="x" && key != "stack"){
          value = (key=="c" ? this.dump_closure(value)
                            : this.dump_obj(value));
          s += "<tr><td>" + key + ": </td>" +
               "<td colspan='3'>" + value + "</td></tr>";
        }
      }, this));
      s += "<tr><td>x:</td><td>" +
           (this.is_opc(obj["x"]) ? this.dump_opc(obj["x"])
                                  : this.dump_obj(obj["x"])) +
           "</td>";

      // stack
      s += "<td style='border-left: 1px solid black'>stack:</td><td>" +
           this.dump_stack(obj["stack"], obj["s"]) +
           "</td></tr>";
      s += "</table>";
    }
    else{
      s = _.escape(inspect(obj)) + "<br>\n";
    }
    var dumpitem = $("<div/>", { class: ("dump" + this.n_dumps) });
    dumpitem.html(s);
    $(this.dumparea).append(dumpitem);
    (_.bind(function(n){
        $(".header", this.dump_el(this.n_dumps)).click(_.bind(function(){
        this.dump_move_to(n);
        this.dump_fold();
      }, this));
    }, this))(this.n_dumps);
    dumpitem.hide();
    this.n_dumps++;
  },

  //
  // UI
  //
  dump_el: function(n) {
    return $(".dump"+n, this.dumparea);
  },
  dump_move_to: function(n){
    if (n < 0) n = this.n_dumps + n;

    if (0 <= n && n <= this.n_dumps){
      this.dump_el(this.cur).hide();
      this.cur = n;
      this.dump_el(this.cur).show();
    }
  },

  dump_move: function(dir){
    if(0 <= this.cur && this.cur < this.n_dumps)
      this.dump_el(this.cur).hide();

    if(0 <= this.cur+dir && this.cur+dir < this.n_dumps)
      this.cur += dir;

    this.dump_el(this.cur).show();
  },

  dump_fold: function(){
    for(var i=0; i<this.n_dumps; i++)
      if(i!=this.cur) this.dump_el(i).hide();

    this.is_folded = true;
  },

  dump_unfold: function(){
    for(var i=0; i<this.n_dumps; i++)
      this.dump_el(i).show();

    this.is_folded = false;
  },

  dump_toggle_fold: function(){
    if(this.is_folded)
      this.dump_unfold();
    else
      this.dump_fold();
  }
});

Dumper.toggle_fold = function(n){
  $(".fold"+n, this.dumparea).toggle();
};

export default Dumper;
