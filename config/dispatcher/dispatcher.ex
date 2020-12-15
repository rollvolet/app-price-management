defmodule Dispatcher do
  use Matcher

  define_accept_types [
    json: [ "application/json", "application/vnd.api+json" ],
    html: [ "text/html", "application/xhtml+html" ],
    any: [ "*/*" ]
  ]

  define_layers [ :static, :api, :frontend_fallback, :not_found ]

  @json %{ accept: %{ json: true }, layer: :api }

  match "/sessions/*path", @json do
    Proxy.forward conn, path, "http://login/sessions/"
  end

  get "/products/search/*path", @json do
    Proxy.forward conn, path, "http://search/products/search/"
  end

  match "/business-entities/*path", @json do
    Proxy.forward conn, path, "http://cache/business-entities/"
  end

  match "/addresses/*path", @json do
    Proxy.forward conn, path, "http://cache/addresses/"
  end

  match "/contact-points/*path", @json do
    Proxy.forward conn, path, "http://cache/contact-points/"
  end

  match "/offerings/*path", @json do
    Proxy.forward conn, path, "http://cache/offerings/"
  end

  match "/products/*path", @json do
    Proxy.forward conn, path, "http://cache/products/"
  end

  match "/unit-price-specifications/*path", @json do
    Proxy.forward conn, path, "http://cache/unit-price-specifications/"
  end

  match "/warehouse-locations/*path", @json do
    Proxy.forward conn, path, "http://cache/warehouse-locations/"
  end

  match "/warehouse-departments/*path", @json do
    Proxy.forward conn, path, "http://cache/warehouse-departments/"
  end

  match "/business-entity-types/*path", @json do
    Proxy.forward conn, path, "http://cache/business-entity-types/"
  end

  match "/unit-codes/*path", @json do
    Proxy.forward conn, path, "http://cache/unit-codes/"
  end

  match "/product-categories/*path", @json do
    Proxy.forward conn, path, "http://cache/product-categories/"
  end

  match "/organization-types/*path", @json do
    Proxy.forward conn, path, "http://cache/organization-types/"
  end

  match "/languages/*path", @json do
    Proxy.forward conn, path, "http://cache/languages/"
  end

  match "/countries/*path", @json do
    Proxy.forward conn, path, "http://cache/countries/"
  end

  post "/files/*path", @json do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", @json do
    Proxy.forward conn, path, "http://file/files/"
  end

  get "/files/:id/dowload", @json do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  get "/files/*path", @json do
    Proxy.forward conn, path, "http://resource/files/"
  end

  patch "/files/*path", @json do
    Proxy.forward conn, path, "http://resource/files/"
  end

  post "/sequence-numbers/*path", @json do
    Proxy.forward conn, path, "http://sequence-numbers/sequence-numbers/"
  end

  get "/assets/*path", %{ layer: :static } do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  get "/*_path", %{ layer: :frontend_fallback } do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  match "/*_path", %{ last_call: true, accept: %{ json: true } } do
    send_resp( conn, 404, "{ \"error\": { \"code\": 404, \"message\": \"Route not found.  See config/dispatcher.ex\" } }" )
  end

  match "/*_path", %{ last_call: true } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
