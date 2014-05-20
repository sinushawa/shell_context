struct LeadingEdge (ID, SlaveFaces, KnotVertex, LoneVertex)
struct SolutionVertex (ID, GhostVertex, GhostEdge)

fn lineLineIntersect p1 p2 p3 p4 = 
(
	local a=p2-p1
	local b=p4-p3
	local c=p3-p1
	local cross1 = cross a b
	local cross2 = cross c b
	p1 + ( a*( (dot cross2 cross1)/((length cross1)^2) ) )
)

fn getSimilarKnotDifferentEdge leadingEdges KnotVertex ID=
(
	diff = #()
	for LeadingEdge in LeadingEdges do
	(
		if LeadingEdge.KnotVertex == KnotVertex AND LeadingEdge.ID != ID then
		(
			append diff LeadingEdge
		)
	)
	return diff
)
fn getSimilarGhost CreatedVertices createdVertex= 
(
	similar = #()
	for vertex in CreatedVertices do
	(
		if vertex.GhostVertex == createdVertex.GhostVertex then
		(
			append similar vertex
		)
	)
	return similar
)
fn getSimilarGhostEdge CreatedVertices createdVertex= 
(
	similar = #()
	for vertex in CreatedVertices do
	(
		if vertex.GhostEdge == createdVertex.GhostEdge then
		(
			append similar vertex
		)
	)
	return similar
)

fn start size=
(
	undo on
	(
		Pol = selection[1].modifiers[#Edit_Poly]
		ED = EditPolyMod.GetSelection Pol #Face
		NbEdges = EditPolyMod.GetNumEdges Pol
		createdVertices=#()
		LeadingEdges = #()
		for i=1 to NbEdges do
		(
			tempFace1 = Pol.EditPolyMod.GetEdgeFace i 1
			tempFace2 = Pol.EditPolyMod.GetEdgeFace i 2
			if tempFace1 == 0 then
			(
				KnotP1 = Pol.EditPolyMod.GetEdgeVertex i 1
				KnotP1Nb = Pol.EditPolyMod.GetVertexEdgeCount KnotP1
				KnotP2 = Pol.EditPolyMod.GetEdgeVertex i 2
				KnotP2Nb = Pol.EditPolyMod.GetVertexEdgeCount KnotP2
				if KnotP1Nb>2 then
				(
					NbFaces = Pol.EditPolyMod.GetVertexFaceCount KnotP1
					SlaveFaces=#()
					for j=1 to NbFaces do
					(
						faceID = Pol.EditPolyMod.GetVertexFace KnotP1 j
						if faceID!=tempFace2 then
						(
							append SlaveFaces faceID
						)
					)
					temp = LeadingEdge ID:i SlaveFaces:SlaveFaces KnotVertex:KnotP1 LoneVertex:KnotP2
					append LeadingEdges temp
				)
				if KnotP2Nb>2 then
				(
					NbFaces = Pol.EditPolyMod.GetVertexFaceCount KnotP2
					SlaveFaces=#()
					for j=1 to NbFaces do
					(
						faceID = Pol.EditPolyMod.GetVertexFace KnotP2 j
						if faceID!=tempFace2 then
						(
							append SlaveFaces faceID
						)
					)
					temp = LeadingEdge ID:i SlaveFaces:SlaveFaces KnotVertex:KnotP2 LoneVertex:KnotP1
					append LeadingEdges temp
				)
			)
			if tempFace2 == 0 then
			(
				KnotP1 = Pol.EditPolyMod.GetEdgeVertex i 1
				KnotP1Nb = Pol.EditPolyMod.GetVertexEdgeCount KnotP1
				KnotP2 = Pol.EditPolyMod.GetEdgeVertex i 2
				KnotP2Nb = Pol.EditPolyMod.GetVertexEdgeCount KnotP2
				if KnotP1Nb>2 then
				(
					NbFaces = Pol.EditPolyMod.GetVertexFaceCount KnotP1
					SlaveFaces=#()
					for j=1 to NbFaces do
					(
						faceID = Pol.EditPolyMod.GetVertexFace KnotP1 j
						if faceID!=tempFace1 then
						(
							append SlaveFaces faceID
						)
					)
					temp = LeadingEdge ID:i SlaveFaces:SlaveFaces KnotVertex:KnotP1 LoneVertex:KnotP2
					append LeadingEdges temp
				)
				if KnotP2Nb>2 then
				(
					NbFaces = Pol.EditPolyMod.GetVertexFaceCount KnotP2
					SlaveFaces=#()
					for j=1 to NbFaces do
					(
						faceID = Pol.EditPolyMod.GetVertexFace KnotP2 j
						if faceID!=tempFace1 then
						(
							append SlaveFaces faceID
						)
					)
					temp = LeadingEdge ID:i SlaveFaces:SlaveFaces KnotVertex:KnotP2 LoneVertex:KnotP1
					append LeadingEdges temp
				)
			)
		)
		
		verticesCount = EditPolyMod.GetNumVertices Pol
		for  LeadingEdge in LeadingEdges do
		(
			print LeadingEdge
			vec = (Pol.EditPolyMod.GetVertex LeadingEdge.LoneVertex) - (Pol.EditPolyMod.GetVertex LeadingEdge.KnotVertex)
			vec = normalize vec
			for SlaveFace in LeadingEdge.SlaveFaces do
			(
				normal=polyop.getFaceNormal selection[1] SlaveFace
				ledLN = dot vec normal
				if ledLN!= 0 then
				(
					vec = vec * (1/ledLN) * size
				)
				else
				(
					vec = normal * size
				)
				verticesOnFace=#()
				NbVerticesInFace = Pol.EditPolyMod.GetFaceDegree SlaveFace
				for i=1 to NbVerticesInFace do
				(
					vertexID = Pol.EditPolyMod.GetFaceVertex SlaveFace i
					append verticesOnFace vertexID
				)
				toShell = getSimilarKnotDifferentEdge leadingEdges LeadingEdge.KnotVertex LeadingEdge.ID
				for i=1 to toShell.Count do
				(
					
					vertexID = toShell[i].LoneVertex
					IDToRemove = findItem verticesOnFace vertexID
					vertexData = Pol.EditPolyMod.GetVertex  vertexID
					temp = SolutionVertex ID:verticesCount GhostVertex:vertexID GhostEdge:LeadingEdge.ID
					append createdVertices temp
					verticesCount = verticesCount+1
					finalpos = (vertexData + vec - selection[1].pos)
					Pol.EditPolyMod.CreateVertex finalpos
				)
				toShell = getSimilarKnotDifferentEdge leadingEdges LeadingEdge.KnotVertex LeadingEdge.ID
				for i=1 to toShell.Count do
				(
					vertexID = toShell[i].KnotVertex
					IDToRemove = findItem verticesOnFace vertexID
					vertexData = Pol.EditPolyMod.GetVertex  vertexID
					temp = SolutionVertex ID:verticesCount GhostVertex:vertexID GhostEdge:LeadingEdge.ID
					append createdVertices temp
					verticesCount = verticesCount+1
					finalpos = (vertexData + vec - selection[1].pos)
					Pol.EditPolyMod.CreateVertex finalpos
				)
				/*
				for i=1 to verticesOnFace.Count do
				(
					vertexID = verticesOnFace[i]
					vertexData = Pol.EditPolyMod.GetVertex  vertexID
					finalpos = (vertexData + (normal * size) - selection[1].pos)
					Pol.EditPolyMod.CreateVertex finalpos
				)
				*/
				/*
				NbVerticesInFace = Pol.EditPolyMod.GetFaceDegree SlaveFace
				for i=1 to NbVerticesInFace do
				(
					vertexID = Pol.EditPolyMod.GetFaceVertex SlaveFace i
					vertexData = Pol.EditPolyMod.GetVertex  vertexID
					finalpos = (vertexData + vec - selection[1].pos)
					Pol.EditPolyMod.CreateVertex finalpos
				)
				*/
			)
		)
		Pol.EditPolyMod.Commit()
		for createdVertex in createdVertices do
		(
			similarGhostVertex = getSimilarGhost CreatedVertices createdVertex
			if similarGhostVertex.Count>1 then
			(
				similarGhostEdge=#()
				for i=1 to similarGhostVertex.Count do
				(
					append similarGhostEdge (getSimilarGhostEdge CreatedVertices createdVertex)
				)
			)
		)
	)
)
start 5