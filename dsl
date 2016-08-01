u = Field(name="velocity", rank=1)
h = Field(name="height", rank=0)
nu = Constant(name="nu")
sigma = Constant(name="sigma")
f = Constant(name="f")
g = Constant(name="g")
tau = Constant(name="tau", rank=1)
e = Permutation()

VelocityEq = Equation(Dt(u), -g * grad(h) + dot(u, grad(u)) + f * inner(e, u) + nu)
HeightEq = Equation(Dt(h), -div(h * u))
TimeStep = Solve(name="step", spatial_order=1, temporal_order=1, equations=[VelocityEq, HeightEq])

m = Mesh(name="shallow_water", dimension=2, fields=[u, h], solves=[TimeStep])
