import someThing from "somePackage";

// Some comment
// describe("asd", () => {});
describe.skip("basket", () => {
  it("works", function () {});
  // describe("asd", () => {});
  describe("Test A", () => {
    it("errors", async () => {
      try {
        await getUser();
      } catch (err) {
        expect(err.error).toBeDefined();
      }
    });
  });

  describe.skip("Test B", () => {
    it(`template string`, async () => {
      it("nests", () => {
        // describe("asd", () => {});
      });
      const res = await getUser();
      expect(res.user).toBeDefined();
    });

    it("Some other thing", () => {});

    test();
  });
});
