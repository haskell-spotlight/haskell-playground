const path = require("path");
const TerserPlugin = require("terser-webpack-plugin");
const HtmlWebpackPlugin = require("html-webpack-plugin");

module.exports = () => {
  const isProd = process.env["NODE_ENV"] === "production";
  return {
    mode: isProd ? "production" : "development",
    entry: path.resolve(__dirname, "./src/index.tsx"),
    optimization: {
      minimize: isProd,
      minimizer: isProd
        ? [
            new TerserPlugin({
              parallel: true,
              terserOptions: {
                // https://github.com/webpack-contrib/terser-webpack-plugin#terseroptions
              },
            }),
          ]
        : [],
      moduleIds: "deterministic",
      usedExports: false,
      runtimeChunk: "single",
      splitChunks: {
        cacheGroups: {
          vendor: {
            test: /[\\/]node_modules[\\/]/,
            name: "vendors",
            chunks: "all",
          },
        },
      },
    },
    devtool: !isProd ? "source-map" : false,
    devServer: !isProd
      ? {
        static: [path.resolve(__dirname, "./dist")],
        port: 4242,

      }
      : undefined,
    plugins: [
      new HtmlWebpackPlugin({
        template: "src/index.html",
      }),
    ],
    module: {
      rules: [
        {
          test: /\.css$/i,
          use: [
            "style-loader",
            {
              loader: "css-loader",
              options: {
                modules: {
                  auto: true,
                  namedExport: true,
                  localIdentName: "[local]--[hash:base64:5]",
                },
              },
            },
          ],
        },
        {
          test: /\.tsx?$/,
          use: {
            loader: "ts-loader",
            options: {
              // disable type checker for faster build in development
              transpileOnly: !isProd,
              configFile: path.resolve(__dirname, "./tsconfig.json"),
            },
          },
          include: path.resolve(__dirname),
        },
      ],
    },
    resolve: {
      extensions: [".wasm", ".tsx", ".ts", ".mjs", ".js", ".json"],
    },
    output: {
      path: path.resolve(__dirname, "./dist"),
      filename: "[name].js",
      library: {
        name: "HSPG",
        type: "umd",
        umdNamedDefine: true,
      }
    },
  };
};
