#include "Entity.h"
#include "Offsets.h"
#include "Memory.h"

namespace Engine
{
	Vector3& Entity::GetPosition()
	{
		if (!Memory::IsValidPtr<Entity>(this))
			return *(new Vector3());

		return *Memory::Ptr<Vector3*>(this, OFFSET_ENTITY_POSITION);
	}

	Vector3& Entity::GetNeck()
	{
		if (!Memory::IsValidPtr<Entity>(this))
			return *(new Vector3());

		return *Memory::Ptr<Vector3*>(this, OFFSET_ENTITY_NECK);
	}

	Vector3& Entity::GetHead()
	{
		if (!Memory::IsValidPtr<Entity>(this))
			return *(new Vector3());

		return *Memory::Ptr<Vector3*>(this, OFFSET_ENTITY_HEAD);
	}

	Vector3& Entity::GetRightHand()
	{
		if (!Memory::IsValidPtr<Entity>(this))
			return *(new Vector3());

		return *Memory::Ptr<Vector3*>(this, OFFSET_ENTITY_RIGHTHAND);
	}

	Vector3& Entity::GetLeftHand()
	{
		if (!Memory::IsValidPtr<Entity>(this))
			return *(new Vector3());

		return *Memory::Ptr<Vector3*>(this, OFFSET_ENTITY_LEFTHAND);
	}

	INT8 Entity::GetTeam()
	{
		if (!Memory::IsValidPtr<Entity>(this))
			return 0;

		auto pEntityInfo = *Memory::Ptr<void**>(this, OFFSET_ENTITY_ENTITYINFO);

		if (!Memory::IsValidPtr(pEntityInfo))
			return 0;

		auto pMainComponent = *Memory::Ptr<void**>(pEntityInfo, OFFSET_ENTITYINFO_MAINCOMPONENT);

		if (!Memory::IsValidPtr(pMainComponent))
			return 0;

		return *Memory::Ptr<INT8*>(pMainComponent, OFFSET_PLAYERINFO_COMPONENT_TEAM);
	}

	std::wstring Entity::GetName() {
		if (!Memory::IsValidPtr<Entity>(this))
			return *(new std::wstring());

		auto pPlayerInfo = *Memory::Ptr<void**>(this, OFFSET_ENTITY_PLAYERINFO);

		if (!Memory::IsValidPtr(pPlayerInfo))
			return *(new std::wstring());

		auto pPlayerInfoDeref = *Memory::Ptr<void**>(pPlayerInfo, OFFSET_ENTITYINFO_MAINCOMPONENT);

		if (!Memory::IsValidPtr(pPlayerInfoDeref))
			return *(new std::wstring());

		return *Memory::Ptr<std::wstring*>(pPlayerInfoDeref, OFFSET_PLAYERINFO_COMPONENT_NAME);
	}

	float Entity::GetHealth()
	{
		if (!Memory::IsValidPtr<Entity>(this))
			return 0.0f;

		auto pEntityInfo = *Memory::Ptr<void**>(this, OFFSET_ENTITY_ENTITYINFO);

		if (!Memory::IsValidPtr(pEntityInfo))
			return 0.0f;

		auto pMainComponent = *Memory::Ptr<void**>(pEntityInfo, OFFSET_ENTITYINFO_MAINCOMPONENT);

		if (!Memory::IsValidPtr(pMainComponent))
			return 0.0f;

		auto pChildComponent = *Memory::Ptr<void**>(pMainComponent, OFFSET_MAINCOMPONENT_CHILDCOMPONENT);

		if (!Memory::IsValidPtr(pChildComponent))
			return 0.0f;

		return *Memory::Ptr<INT8 *>(pChildComponent, OFFSET_CHILDCOMPONENT_HEALTH);
	}

	Vector4& Entity::GetViewAngle()
	{
		if (!Memory::IsValidPtr<Entity>(this))
			return *(new Vector4());

		auto viewAngle1 = *Memory::Ptr<void**>(this, OFFSET_MISC_PLAYER_VIEWANGLE_1);

		if (!Memory::IsValidPtr(viewAngle1))
			return *(new Vector4());

		auto viewAngle2 = *Memory::Ptr<void**>(this, OFFSET_MISC_PLAYER_VIEWANGLE_2);

		if (!Memory::IsValidPtr(viewAngle2))
			return *(new Vector4());

		return *Memory::Ptr<Vector4*>(this, OFFSET_MISC_PLAYER_VIEWANGLE_3);
	}

	float Entity::GetDistance(float Xx, float Yy, float xX, float yY) {
		return sqrt((yY - Yy) * (yY - Yy) + (xX - Xx) * (xX - Xx));
	}
}