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


  ## Static frontend resources of all kinds

  get "/favicon.ico", %{ layer: :static } do
    Proxy.forward conn, [], "http://frontend/favicon.ico"
  end

  get "/assets/*path", %{ layer: :static } do
    Proxy.forward conn, path, "http://frontend/assets/"
  end

  get "/torii/redirect.html", %{ layer: :static } do
    Proxy.forward conn, [], "http://frontend/torii/redirect.html"
  end


  ## Authentication / login

  match "/sessions/*path", @json_service do
    Proxy.forward conn, path, "http://msal-login/sessions/"
  end

  match "/mock-sessions/*path", @json_service do
    Proxy.forward conn, path, "http://mock-login/sessions/"
  end

  get "/accounts/*path", @json_service do
    Proxy.forward conn, path, "http://cache/accounts/"
  end

  get "/users/*path", @json_service do
    Proxy.forward conn, path, "http://cache/users/"
  end

  ## File upload/download

  get "/files/:id/download", %{ layer: :static } do
    Proxy.forward conn, [], "http://file/files/" <> id <> "/download"
  end

  post "/files/*path", @json_service do
    Proxy.forward conn, path, "http://file/files/"
  end

  delete "/files/*path", @json_service do
    Proxy.forward conn, path, "http://file/files/"
  end

  get "/files/*path", @json_service do
    Proxy.forward conn, path, "http://resource/files/"
  end

  patch "/files/*path", @json_service do
    Proxy.forward conn, path, "http://resource/files/"
  end


  ## Sequence numbers

  post "/sequence-numbers/*path", @json_service do
    Proxy.forward conn, path, "http://sequence-numbers/sequence-numbers/"
  end


  ## Search

  get "/products/search/*path", @json_service do
    Proxy.forward conn, path, "http://search/products/search/"
  end


  ## Regular resources

  match "/business-entities/*path", @json_service do
    Proxy.forward conn, path, "http://cache/business-entities/"
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

  match "/business-entity-types/*path", @json_service do
    Proxy.forward conn, path, "http://cache/business-entity-types/"
  end

  match "/product-categories/*path", @json_service do
    Proxy.forward conn, path, "http://cache/product-categories/"
  end

  get "/warehouse-departments/*path", @json_service do
    Proxy.forward conn, path, "http://cache/warehouse-departments/"
  end

  get "/unit-codes/*path", @json_service do
    Proxy.forward conn, path, "http://cache/unit-codes/"
  end


  ## Fallback

  get "/*_path", @html_pages do
    Proxy.forward conn, [], "http://frontend/index.html"
  end

  match "/*_path", %{ last_call: true, accept: %{ json: true } } do
    send_resp( conn, 404, "{ \"error\": { \"code\": 404, \"message\": \"Route not found.  See config/dispatcher.ex\" } }" )
  end

  match "/*_path", %{ last_call: true } do
    send_resp( conn, 404, "Route not found.  See config/dispatcher.ex" )
  end

end
