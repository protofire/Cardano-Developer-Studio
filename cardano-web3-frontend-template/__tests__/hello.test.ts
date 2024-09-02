// __tests__/hello.test.ts
import { createMocks } from 'node-mocks-http';
import handler from '../src/pages/api/hello';

describe('/api/hello', () => {
  it('returns a 200 status code and message', () => {
    const { req, res } = createMocks();

    handler(req, res);

    expect(res._getStatusCode()).toBe(200);
    expect(res._getData()).toEqual(JSON.stringify({ message: 'Hello, World!' }));
  });
});
