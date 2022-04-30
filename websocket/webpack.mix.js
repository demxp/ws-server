const webpack = require('webpack');
let mix = require('laravel-mix');

mix.webpackConfig({
  plugins: [
    new webpack.ContextReplacementPlugin(/moment[/\\]locale$/, /en-us|ru/)
  ]
})

mix.setPublicPath('priv/www/');

mix.js('src-frontend/assets/js/appadmin.js', 'priv/www/static/js')
.extract(['vue', 'jquery', 'moment', 'sweetalert']).version();


mix.styles([
  'src-frontend/assets/admin/bootstrap/css/bootstrap.min.css',
  'src-frontend/assets/admin/font-awesome/4.5.0/css/font-awesome.min.css',
  'src-frontend/assets/admin/ionicons/2.0.1/css/ionicons.min.css',
  'src-frontend/assets/admin/dist/css/skins/_all-skins.min.css',
  'src-frontend/assets/admin/dist/css/AdminLTE.min.css', 
  'src-frontend/assets/admin/dist/css/style_add.css'
], 'priv/www/static/css/admin.css').version();

mix.scripts([
  'src-frontend/assets/admin/bootstrap/js/bootstrap.min.js',
  'src-frontend/assets/admin/dist/js/app.min.js',
  'src-frontend/assets/admin/dist/js/promise.js',
], 'priv/www/static/js/admin.js').version();

mix.copy('src-frontend/assets/admin/bootstrap/fonts','priv/www/static/fonts');
mix.copy('src-frontend/assets/admin/dist/img', 'priv/www/static/img');
mix.copy('priv/www/static', '_rel/websocket_release/lib/websocket-rolling/priv/www/static');