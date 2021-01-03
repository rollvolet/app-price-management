defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ],
    html: [ "text/html", "application/xhtml+html" ],
    any: [ "*/*" ]
  ]

  define_layers [ :static, :frontend, :api ]

  @json_service %{ layer: :api, accept: %{ json: true } }
  @html_pages %{ layer: :frontend, accept: %{ html: true } }

  get "/assets/*path", %{ layer: :static } do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  get "/torii/redirect.html", %{ layer: :static } do
    Proxy.forward conn, [], "http://frontend/torii/redirect.html"
  end

  get "/*_path", @html_pages do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  match "/sessions/*path", @json_service do
    Proxy.forward conn, path, "http://msal-login/sessions/"
  end

  get "/products/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/products/search/"
  end

  match "/business-entities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/business-entities/"
  end

  match "/addresses/*path", @json_service do
    Proxy.forward conn, path, "http://cache/addresses/"
  end

  match "/contact-points/*path", @json_service do
    Proxy.forward conn, path, "http://cache/contact-points/"
  end

  match "/offerings/*path", @json_service do
    Proxy.forward conn, path, "http://cache/offerings/"
  end

  match "/products/*path", @json_service do
    Proxy.forward conn, path, "http://cache/products/"
  end

  match "/unit-price-specifications/*path", @json_service do
    Proxy.forward conn, path, "http://cache/unit-price-specifications/"
  end

  match "/warehouse-locations/*path", @json_service do
    Proxy.forward conn, path, "http://cache/warehouse-locations/"
  end

  match "/warehouse-departments/*path", @json_service do
    Proxy.forward conn, path, "http://cache/warehouse-departments/"
  end

  match "/business-entity-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/business-entity-types/"
  end

  match "/unit-codes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/unit-codes/"
  end

  match "/product-categories/*path", @json_service do
    Proxy.forward conn, path, "http://cache/product-categories/"
  end

  match "/organization-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/organization-types/"
  end

  match "/languages/*path", @json_service do
    Proxy.forward conn, path, "http://cache/languages/"
  end

  match "/countries/*path", @json_service do
    Proxy.forward conn, path, "http://cache/countries/"
  end

  post "/files/*path", @json_service do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", @json_service do
    Proxy.forward conn, path, "http://file/files/"
  end

  get "/files/:id/dowload", @json_service do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  get "/files/*path", @json_service do
    Proxy.forward conn, path, "http://resource/files/"
  end

  patch "/files/*path", @json_service do
    Proxy.forward conn, path, "http://resource/files/"
  end

  post "/sequence-numbers/*path", @json_service do
    Proxy.forward conn, path, "http://sequence-numbers/sequence-numbers/"
  end

  match "/*_path", %{ last_call: true, accept: %{ json: true } } do
    send_resp( conn, 404, "{ \"error\": { \"code\": 404, \"message\": \"Route not found.  See config/dispatcher.ex\" } }" )
  end

  match "/*_path", %{ last_call: true } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
