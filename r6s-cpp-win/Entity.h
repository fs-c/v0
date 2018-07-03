#pragma once
#include "Vectors.h"
#include "stdafx.h"

namespace Engine
{
	class Entity
	{
		public:
			Vector3& GetPosition();
			Vector3& GetNeck();
			Vector3& GetHead();
			Vector3& GetRightHand();
			Vector3& GetLeftHand();
			INT8 GetTeam();
			std::wstring GetName();
			float GetHealth();
			Vector4& GetViewAngle();
			float GetDistance(float Xc, float Yy, float xX, float yY);
    };
}
