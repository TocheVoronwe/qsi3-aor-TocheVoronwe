open Jest;

open Expect;

open Tennis;

describe("Test tennis transitions", () =>
  test("Given deuce when player wins then score is correct", () =>
    expect(scoreWhenDeuce(PlayerOne)) |> toEqual(Advantage(PlayerOne))
  )
);
