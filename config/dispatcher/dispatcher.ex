defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ],
    html: [ "text/html", "application/xhtml+html" ],
    any: [ "*/*" ]
  ]

  @html %{ accept: %{ html: true } }
  @json %{ accept: %{ json: true } }
  @any %{ accept: %{ any: true } }

  match "/business-entities/*path", @any do
    Proxy.forward conn, path, "http://cache/business-entities/"
  end

  match "/offerings/*path", @any do
    Proxy.forward conn, path, "http://cache/offerings/"
  end

  match "/products/*path", @any do
    Proxy.forward conn, path, "http://cache/products/"
  end

  match "/unit-price-specifications/*path", @any do
    Proxy.forward conn, path, "http://cache/unit-price-specifications/"
  end

  match "/warehouse-locations/*path", @any do
    Proxy.forward conn, path, "http://cache/warehouse-locations/"
  end

  match "/business-entity-types/*path", @any do
    Proxy.forward conn, path, "http://cache/business-entity-types/"
  end

  match "/unit-codes/*path", @any do
    Proxy.forward conn, path, "http://cache/unit-codes/"
  end

  match "/product-categories/*path", @any do
    Proxy.forward conn, path, "http://cache/product-categories/"
  end

  match "_", %{ last_call: true, accept: %{ json: true } } do
    send_resp( conn, 404, "{ \"error\": { \"code\": 404, \"message\": \"Route not found.  See config/dispatcher.ex\" } }" )
  end

  match "_", %{ last_call: true, accept: %{ any: true } } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
