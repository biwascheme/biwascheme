#!/usr/bin/env ruby
ENV["GEM_HOME"] = "/home/yhara/gems/"
require 'cgi'
require 'rubygems'
require 'textgraph'
require 'json'

class Array
  def to_sexp
    items = self.map{|item|
      case item
      when true
        "#t"
      when false
        "#f"
      when Array
        item.to_sexp
      when Symbol
        item == :dot ? "." : "'#{item}"
      else
        item.inspect
      end
    }.join(" ")
    ["(", items  , ")"].join
  end
end


class TextGraphAPI

  def initialize(format)
    @format = %w(json xml sexp).include?(format) ? format : "json"
  end

  def parse(text)
    begin 
      @tg = TextGraph.parse(text)
    rescue
      @tg = nil
    end
    self.__send__("output_#{@format}")
  end

  def output_json
    if @tg
      obj = {"succeed" => true}
      obj["links"] = @tg.links
      obj["cells"] = @tg.cells.map{|cell|
        {"w" => cell.w,
         "h" => cell.h,
         "x" => cell.x,
         "y" => cell.y,
         "raw_content" => cell.raw_content}
      }
    else
      obj = {"succeed" => false}
    end

    JSON.generate(obj)
  end

  def output_sexp
    if @tg
      [
        ["links", :dot, @tg.links],
        ["cells", :dot, @tg.cells.map{|cell|
          [cell.x, cell.y, cell.w, cell.h, cell.raw_content.strip]
        }]
      ].to_sexp
    else
      #f
    end
  end

end

cgi = CGI.new
text   = cgi.params["text"][0]
format = cgi.params["format"][0]

cgi.out{
  TextGraphAPI.new(format).parse(text) 
}
