library(httr2)

vimeo_client <- 
  function(){
    oauth_client(
      id = "8cce4e6d63ddbda11e3b566cd158f5f97fa27a6d",
      secret = obfuscated("1TRraIdc1hzYXSwfXHnsvqmRNxe26jINZDOj4QHy_BLkj3u4dpCbGFbARlgx1agKyBFHzEW2yKZLRwLG8FSYqAm5jxEY9KgD-wX0Y8AP_tFSJAj-kNZYDsaER8EdVGhWnBDIIzQRk1kWqMPVUEJLBCEABLSpmfNv7r073uXG2Otlb1WzNaGHTMC90dKaFEzy"),
      token_url = "https://api.vimeo.com/oauth/access_token",
      name = "usdm-archive"
    )
  }


token <- 
  oauth_flow_auth_code(
  client = vimeo_client(),
  auth_url = "https://api.vimeo.com/oauth/authorize"
)
