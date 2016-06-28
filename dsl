m = Mesh(name="shallow_water", dimension=2)
V = Field(mesh=m, name="velocity", rank=1, staggered=False)
H = Field(mesh=m, name="height", rank=0, staggered=True)

spatial_stagger=[None, All, Index]
temporal_stagger=True

VelocityUpdate = FieldUpdate(mesh=m, V, V)

U = Update(mesh=m, name="step", spatial_order=1, temporal_order=1, updates=[VelocityUpdate])



