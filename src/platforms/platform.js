const Platform = {
  // Return true if running on Node.js
  isNode: () => typeof global !== 'undefined',
  // Return true if running on a browser
  isBrowser: () => typeof window !== 'undefined',
}

export default Platform;
