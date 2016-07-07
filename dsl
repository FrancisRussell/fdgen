V = Field(name="velocity", rank=1, staggered=False)
H = Field(name="height", rank=0, staggered=True)
m = Mesh(name="shallow_water", dimension=2, fields=[V, H])

VelocityUpdate = FieldUpdate(V, V)

#U = Update(mesh=m, name="step", spatial_order=1, temporal_order=1, updates=[VelocityUpdate])



