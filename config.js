const config = {
  gatsby: {
    pathPrefix: "/",
    siteUrl: "https://splashkit.io",
    trailingSlash: false,
  },
  header: {
    logo: "https://www.splashkit.io/images/skbox-5d36169d.svg",
    logoLink: "https://splashkit.io/arcana/",
    title:
      "Programming Arcana",
    githubUrl: "https://github.com/splashkit/programming-arcana",
    helpUrl: "https://github.com/splashkit/splashkit-core/discussions",
    tweetText:
      "Check out learning to program with the Programming Arcana",
    links: [
      {
        text: "test",
        link: "test",
      },
    ],
    search: {
      enabled: false,
      indexName: "programming-arcana",
      algoliaAppId: process.env.GATSBY_ALGOLIA_APP_ID,
      algoliaSearchKey: process.env.GATSBY_ALGOLIA_SEARCH_KEY,
      algoliaAdminKey: process.env.ALGOLIA_ADMIN_KEY,
    },
  },
  sidebar: {
    forcedNavOrder: [
      "index",
      "/01-use-programs-like-a-pro",
      "/02-compile-and-run-programs",
      "/03-build-simple-sequences",
      "/04-storing-changing-values",
      "/05-communicating-syntax",
      "/06-create-reusable-code",
      "/07-control-the-flow",
      "/08-structure-your-data",
      "/09-utilise-indirection",
      "/10-work-with-multiples",
      "/11-organise-multiples-in-collections",
      "/12-develop-a-procedural-program",
      "/13-switch-languages",
      "/14-create-objects",
      "/15-toolbox"
    ],
    "collapsedNav": [
      "/01-use-programs-like-a-pro",
      "/02-compile-and-run-programs",
      "/03-build-simple-sequences",
      "/04-storing-changing-values",
      "/05-communicating-syntax",
      "/06-create-reusable-code",
      "/07-control-the-flow",
      "/08-structure-your-data",
      "/09-utilise-indirection",
      "/10-work-with-multiples",
      "/11-organise-multiples-in-collections",
      "/12-develop-a-procedural-program",
      "/13-switch-languages",
      "/14-create-objects",
      "/15-toolbox"
    ],
    links: [
      {
        text: "SplashKit Docs",
        link: "https://splashkit.io",
      }
    ],
    frontline: false,
    ignoreIndex: false,
  },
  siteMetadata: {
    title: "Learn to Program | Programming Arcana",
    description:
      "A concept focused introduction to programming",
    ogImage:
      null,
    docsLocation:
      "https://splashkit.io/arcana",
    favicon:
      "https://www.splashkit.io/images/favicon.ico",
  },
};

module.exports = config;
