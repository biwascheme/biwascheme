# config.ru - View BiwaScheme website with Pow (http://pow.cx/)
require 'rack'
require 'rack/directory'

use Rack::CommonLogger
use Rack::ShowExceptions
run Rack::Directory.new(".")
