(html-doctype)
(html
  (head
    (title "BiwaScheme : Scheme interpreter for browsers")
    (meta :http-equiv "Content-Type"
          :content "text/html; charset=utf-8" )

    (link :href "css/jquery.mobile-1.0a2.min.css" :rel "stylesheet" :type "text/css")
    (script :src "js/jquery-1.4.4.min.js" :type "text/javascript")
    (script :type "text/javascript"
            "var j = jQuery.noConflict();")
    (script :src "js/jquery.mobile-1.0a2.min.js" :type "text/javascript")

    (script :src "repos/lib/biwascheme.js" :type "text/javascript"))

  (body
    (div :data-role "page" :data-theme "e"
         (div :data-role "header"
              (h1 "iBiwaScheme"))

         (div :data-role "content"
              (div :data-role "fieldcontain"
                   (textarea :cols 40 :rows 8 :id "src" "(+ 1 2)"))
              
              (label "result:")
              (div :id "bs-console")
         )

         (div :data-role "footer"
              (h4
              "Powered by "
              (a :href "http://www.biwascheme.org"
                 "BiwaScheme") (br)
              " version " (span :id "ver" "--")
              ))
     )

    (script :type "text/javascript"
            "j(function(){
                j('#ver').text(BiwaScheme.Version);

                var intp = new BiwaScheme.Interpreter(function(e){
                                                       puts(e);
                                                       });
                j('#src').change(function(){
                  j('#bs-console').empty();
                  var src = j('#src').val();
                  intp.evaluate(src, function(result){
                                     puts(result);
                                     });
                }).trigger('change');
              })")
    ))

