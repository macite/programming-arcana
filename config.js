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
      "/00-introduction",
      "/01-use-programs-like-a-pro",
      "/02-compile-and-run-programs",
      "/03-build-simple-sequences",
      "/04-parameterisation",
      "/05-control-the-flow",
      "/05-structure-data",
      "/06-work-with-multiples",
      "/07-utilise-indirection",
      "/08-project-1",
      "/09-use-other-languages",
      "/10-use-objects",
      "/11-advanced-collections",
      "/12-empower-abstraction",
      "/13-project-2",
      "/15-toolbox"
    ],
    "collapsedNav": [
        "/03-build-simple-sequences/07-in-action"
      // "/00-introduction",
      // "/01-use-programs-like-a-pro",
      // "/02-build-simple-sequences",
      // "/03-control-the-flow",
      // "/04-structure-data",
      // "/05-work-with-multiples",
      // "/06-utilise-indirection",
      // "/07-project-1",
      // "/08-use-other-languages",
      // "/09-use-objects",
      // "/10-advanced-collections",
      // "/11-empower-abstraction",
      // "/12-project-2",
      // "/15-toolbox"
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
