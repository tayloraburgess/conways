module.exports = {
  entry: './src/app.js',
  output: {
    filename: 'bundle.js' 
  },
  module: {
    preLoaders: [
      {
        test: /\.js$/,
        exlude: /node_modules/,
        loader: 'eslint-loader'
      }
    ],
    loaders: [
     {  
       test: /\.js$/,
       exclude: /node_modules/,
       loader: 'babel-loader',
       query: {
         presets: ['react', 'es2015']
       }
     }
    ]
  },
  resolve: {
    extensions: ['', '.js']
  },
  devServer: {
    contentBase: './public',
    hot: true
  },
  watch: true
}
