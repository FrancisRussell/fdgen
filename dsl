V = Field(name="velocity", rank=1)
H = Field(name="height", rank=0)
m = Mesh(name="shallow_water", dimension=2, fields=[V, H])

VelocityUpdate = FieldUpdate(Dt(V), V)

#U = Update(mesh=m, name="step", spatial_order=1, temporal_order=1, updates=[VelocityUpdate])



